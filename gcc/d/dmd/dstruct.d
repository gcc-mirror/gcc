/**
 * Struct and union declarations.
 *
 * Specification: $(LINK2 https://dlang.org/spec/struct.html, Structs, Unions)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dstruct.d, _dstruct.d)
 * Documentation:  https://dlang.org/phobos/dmd_dstruct.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dstruct.d
 */

module dmd.dstruct;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem : search, setFieldOffset;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.opover;
import dmd.target;
import dmd.tokens;
import dmd.typesem : isZeroInit, merge, size, hasPointers;
import dmd.typinf;
import dmd.visitor;

/***************************************
 * Search sd for a member function of the form:
 *   `extern (D) string toString();`
 * Params:
 *   sd = struct declaration to search
 * Returns:
 *   FuncDeclaration of `toString()` if found, `null` if not
 */
FuncDeclaration search_toString(StructDeclaration sd)
{
    Dsymbol s = search_function(sd, Id.tostring);
    FuncDeclaration fd = s ? s.isFuncDeclaration() : null;
    if (fd)
    {
        __gshared TypeFunction tftostring;
        if (!tftostring)
        {
            tftostring = new TypeFunction(ParameterList(), Type.tstring, LINK.d);
            tftostring = tftostring.merge().toTypeFunction();
        }
        fd = fd.overloadExactMatch(tftostring);
    }
    return fd;
}

enum StructFlags : int
{
    none        = 0x0,
    hasPointers = 0x1, // NB: should use noPointers as in ClassFlags
}

/***********************************************************
 * All `struct` declarations are an instance of this.
 */
extern (C++) class StructDeclaration : AggregateDeclaration
{
    FuncDeclarations postblits; // Array of postblit functions
    FuncDeclaration postblit;   // aggregate postblit

    FuncDeclaration xeq;        // TypeInfo_Struct.xopEquals
    FuncDeclaration xcmp;       // TypeInfo_Struct.xopCmp
    FuncDeclaration xhash;      // TypeInfo_Struct.xtoHash
    extern (C++) __gshared FuncDeclaration xerreq;   // object.xopEquals
    extern (C++) __gshared FuncDeclaration xerrcmp;  // object.xopCmp

    // ABI-specific type(s) if the struct can be passed in registers
    TypeTuple argTypes;

    structalign_t alignment;    // alignment applied outside of the struct
    ThreeState ispod;           // if struct is POD

    // `bool` fields that are compacted into bit fields in a string mixin
    private extern (D) static struct BitFields
    {
        bool zeroInit;              // !=0 if initialize with 0 fill
        bool hasIdentityAssign;     // true if has identity opAssign
        bool hasBlitAssign;         // true if opAssign is a blit
        bool hasIdentityEquals;     // true if has identity opEquals
        bool hasNoFields;           // has no fields
        bool hasCopyCtor;           // copy constructor
        bool hasMoveCtor;           // move constructor
        bool hasPointerField;       // members with indirections
        bool hasVoidInitPointers;   // void-initialized unsafe fields
        bool hasUnsafeBitpatterns;  // @system members, pointers, bool
        bool hasFieldWithInvariant; // invariants
        bool computedTypeProperties;// the above 3 fields are computed
        // Even if struct is defined as non-root symbol, some built-in operations
        // (e.g. TypeidExp, NewExp, ArrayLiteralExp, etc) request its TypeInfo.
        // For those, today TypeInfo_Struct is generated in COMDAT.
        bool requestTypeInfo;
    }

    import dmd.common.bitfields : generateBitFields;
    mixin(generateBitFields!(BitFields, ushort));

    extern (D) this(Loc loc, Identifier id, bool inObject)
    {
        super(loc, id);
        this.dsym = DSYM.structDeclaration;
        zeroInit = false; // assume false until we do semantic processing
        ispod = ThreeState.none;
        // For forward references
        type = new TypeStruct(this);

        if (inObject)
        {
            if (id == Id.ModuleInfo && !Module.moduleinfo)
                Module.moduleinfo = this;
        }
    }

    static StructDeclaration create(Loc loc, Identifier id, bool inObject)
    {
        return new StructDeclaration(loc, id, inObject);
    }

    override StructDeclaration syntaxCopy(Dsymbol s)
    {
        StructDeclaration sd =
            s ? cast(StructDeclaration)s
              : new StructDeclaration(loc, ident, false);
        ScopeDsymbol.syntaxCopy(sd);
        return sd;
    }

    override const(char)* kind() const
    {
        return "struct";
    }

    /// Compute cached type properties for `TypeStruct`
    extern(D) final void determineTypeProperties()
    {
        if (computedTypeProperties)
            return;
        foreach (vd; fields)
        {
            import dmd.dsymbolsem : hasPointers;
            if (vd.storage_class & STC.ref_ || vd.hasPointers())
            {
                hasPointerField = true;
                hasUnsafeBitpatterns = true;
            }

            if (vd._init && vd._init.isVoidInitializer() && vd.hasPointers())
                hasVoidInitPointers = true;

            if (vd.storage_class & STC.system || vd.type.hasUnsafeBitpatterns())
                hasUnsafeBitpatterns = true;

            if (!vd._init && vd.type.hasVoidInitPointers())
                hasVoidInitPointers = true;

            if (vd.type.hasInvariant())
                hasFieldWithInvariant = true;
        }
        computedTypeProperties = true;
    }

    /***************************************
     * Determine if struct is POD (Plain Old Data).
     *
     * POD is defined as:
     *      $(OL
     *      $(LI not nested)
     *      $(LI no postblits, destructors, or assignment operators)
     *      $(LI no `ref` fields or fields that are themselves non-POD)
     *      )
     * The idea being these are compatible with C structs.
     *
     * Returns:
     *     true if struct is POD
     */
    final bool isPOD()
    {
        // If we've already determined whether this struct is POD.
        if (ispod != ThreeState.none)
            return (ispod == ThreeState.yes);

        import dmd.clone;

        bool hasCpCtorLocal;
        bool hasMoveCtorLocal;
        bool needCopyCtor;
        bool needMoveCtor;
        needCopyOrMoveCtor(this, hasCpCtorLocal, hasMoveCtorLocal, needCopyCtor, needMoveCtor);

        if (enclosing                      || // is nested
            search(this, loc, Id.postblit) || // has postblit
            search(this, loc, Id.dtor)     || // has destructor
            /* This is commented out because otherwise buildkite vibe.d:
               `canCAS!Task` fails to compile
             */
            //hasMoveCtorLocal               || // has move constructor
            hasCpCtorLocal)                   // has copy constructor
        {
            ispod = ThreeState.no;
            return false;
        }

        // Recursively check all fields are POD.
        for (size_t i = 0; i < fields.length; i++)
        {
            VarDeclaration v = fields[i];
            if (v.storage_class & STC.ref_)
            {
                ispod = ThreeState.no;
                return false;
            }

            if (auto ts = v.type.baseElemOf().isTypeStruct())
            {
                if (!ts.sym.isPOD())
                {
                    ispod = ThreeState.no;
                    return false;
                }
            }
        }

        ispod = ThreeState.yes;
        return true;
    }

    /***************************************
     * Determine if struct has copy construction (copy constructor or postblit)
     * Returns:
     *     true if struct has copy construction
     */
    final bool hasCopyConstruction()
    {
        return postblit || hasCopyCtor;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    final uint numArgTypes() const
    {
        return argTypes && argTypes.arguments ? cast(uint) argTypes.arguments.length : 0;
    }

    final Type argType(uint index)
    {
        return index < numArgTypes() ? (*argTypes.arguments)[index].type : null;
    }


    /***************************************
     * Verifies whether the struct declaration has a
     * constructor that is not a copy constructor.
     * Optionally, it can check whether the struct
     * declaration has a regular constructor, that
     * is not disabled.
     *
     * Params:
     *      ignoreDisabled = true to ignore disabled constructors
     * Returns:
     *      true, if the struct has a regular (optionally,
     *      not disabled) constructor, false otherwise.
     */
    final bool hasRegularCtor(bool ignoreDisabled = false)
    {
        if (!ctor)
            return false;

        bool result;
        overloadApply(ctor, (Dsymbol s)
        {
            if (auto td = s.isTemplateDeclaration())
            {
                if (ignoreDisabled && td.onemember)
                {
                    if (auto ctorDecl = td.onemember.isCtorDeclaration())
                    {
                        if (ctorDecl.storage_class & STC.disable)
                            return 0;
                    }
                }
                result = true;
                return 1;
            }
            if (auto ctorDecl = s.isCtorDeclaration())
            {
                if (!ctorDecl.isCpCtor && (!ignoreDisabled || !(ctorDecl.storage_class & STC.disable)))
                {
                    result = true;
                    return 1;
                }
            }
            return 0;
        });
        return result;
    }
}


/***********************************************************
 * Unions are a variation on structs.
 */
extern (C++) final class UnionDeclaration : StructDeclaration
{
    extern (D) this(Loc loc, Identifier id)
    {
        super(loc, id, false);
        this.dsym = DSYM.unionDeclaration;
    }

    override UnionDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto ud = new UnionDeclaration(loc, ident);
        StructDeclaration.syntaxCopy(ud);
        return ud;
    }

    override const(char)* kind() const
    {
        return "union";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
