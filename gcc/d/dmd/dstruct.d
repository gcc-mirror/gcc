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
import dmd.dmodule;
import dmd.dsymbol;
import dmd.func;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.visitor;

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
