/**
 * Struct and union declarations.
 *
 * Specification: $(LINK2 https://dlang.org/spec/struct.html, Structs, Unions)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dstruct.d, _dstruct.d)
 * Documentation:  https://dlang.org/phobos/dmd_dstruct.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dstruct.d
 */

module dmd.dstruct;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.opover;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
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
extern (C++) FuncDeclaration search_toString(StructDeclaration sd)
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

/***************************************
 * Request additional semantic analysis for TypeInfo generation.
 * Params:
 *      sc = context
 *      t = type that TypeInfo is being generated for
 */
extern (C++) void semanticTypeInfo(Scope* sc, Type t)
{
    if (sc)
    {
        if (sc.intypeof)
            return;
        if (sc.flags & (SCOPE.ctfe | SCOPE.compile | SCOPE.ctfeBlock))
            return;
    }

    if (!t)
        return;

    void visitVector(TypeVector t)
    {
        semanticTypeInfo(sc, t.basetype);
    }

    void visitAArray(TypeAArray t)
    {
        semanticTypeInfo(sc, t.index);
        semanticTypeInfo(sc, t.next);
    }

    void visitStruct(TypeStruct t)
    {
        //printf("semanticTypeInfo.visit(TypeStruct = %s)\n", t.toChars());
        StructDeclaration sd = t.sym;

        /* Step 1: create TypeInfoDeclaration
         */
        if (!sc) // inline may request TypeInfo.
        {
            Scope scx;
            scx._module = sd.getModule();
            getTypeInfoType(sd.loc, t, &scx);
            sd.requestTypeInfo = true;
        }
        else if (!sc.minst)
        {
            // don't yet have to generate TypeInfo instance if
            // the typeid(T) expression exists in speculative scope.
        }
        else
        {
            getTypeInfoType(sd.loc, t, sc);
            sd.requestTypeInfo = true;

            // https://issues.dlang.org/show_bug.cgi?id=15149
            // if the typeid operand type comes from a
            // result of auto function, it may be yet speculative.
            // unSpeculative(sc, sd);
        }

        /* Step 2: If the TypeInfo generation requires sd.semantic3, run it later.
         * This should be done even if typeid(T) exists in speculative scope.
         * Because it may appear later in non-speculative scope.
         */
        if (!sd.members)
            return; // opaque struct
        if (!sd.xeq && !sd.xcmp && !sd.postblit && !sd.tidtor && !sd.xhash && !search_toString(sd))
            return; // none of TypeInfo-specific members

        // If the struct is in a non-root module, run semantic3 to get
        // correct symbols for the member function.
        if (sd.semanticRun >= PASS.semantic3)
        {
            // semantic3 is already done
        }
        else if (TemplateInstance ti = sd.isInstantiated())
        {
            if (ti.minst && !ti.minst.isRoot())
                Module.addDeferredSemantic3(sd);
        }
        else
        {
            if (sd.inNonRoot())
            {
                //printf("deferred sem3 for TypeInfo - sd = %s, inNonRoot = %d\n", sd.toChars(), sd.inNonRoot());
                Module.addDeferredSemantic3(sd);
            }
        }
    }

    void visitTuple(TypeTuple t)
    {
        if (t.arguments)
        {
            foreach (arg; *t.arguments)
            {
                semanticTypeInfo(sc, arg.type);
            }
        }
    }

    /* Note structural similarity of this Type walker to that in isSpeculativeType()
     */

    Type tb = t.toBasetype();
    switch (tb.ty)
    {
        case Tvector:   visitVector(tb.isTypeVector()); break;
        case Taarray:   visitAArray(tb.isTypeAArray()); break;
        case Tstruct:   visitStruct(tb.isTypeStruct()); break;
        case Ttuple:    visitTuple (tb.isTypeTuple());  break;

        case Tclass:
        case Tenum:     break;

        default:        semanticTypeInfo(sc, tb.nextOf()); break;
    }
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
        bool hasPointerField;       // members with indirections
        bool hasVoidInitPointers;   // void-initialized unsafe fields
        bool hasSystemFields;      // @system members
        bool hasFieldWithInvariant; // invariants
        bool computedTypeProperties;// the above 3 fields are computed
        // Even if struct is defined as non-root symbol, some built-in operations
        // (e.g. TypeidExp, NewExp, ArrayLiteralExp, etc) request its TypeInfo.
        // For those, today TypeInfo_Struct is generated in COMDAT.
        bool requestTypeInfo;
    }

    import dmd.common.bitfields : generateBitFields;
    mixin(generateBitFields!(BitFields, ushort));

    extern (D) this(const ref Loc loc, Identifier id, bool inObject)
    {
        super(loc, id);
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

    static StructDeclaration create(const ref Loc loc, Identifier id, bool inObject)
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

    override final Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        //printf("%s.StructDeclaration::search('%s', flags = x%x)\n", toChars(), ident.toChars(), flags);
        if (_scope && !symtab)
            dsymbolSemantic(this, _scope);

        if (!members || !symtab) // opaque or semantic() is not yet called
        {
            // .stringof is always defined (but may be hidden by some other symbol)
            if(ident != Id.stringof && !(flags & IgnoreErrors) && semanticRun < PASS.semanticdone)
                error("is forward referenced when looking for `%s`", ident.toChars());
            return null;
        }

        return ScopeDsymbol.search(loc, ident, flags);
    }

    override const(char)* kind() const
    {
        return "struct";
    }

    override final void finalizeSize()
    {
        //printf("StructDeclaration::finalizeSize() %s, sizeok = %d\n", toChars(), sizeok);
        assert(sizeok != Sizeok.done);

        if (sizeok == Sizeok.inProcess)
        {
            return;
        }
        sizeok = Sizeok.inProcess;

        //printf("+StructDeclaration::finalizeSize() %s, fields.length = %d, sizeok = %d\n", toChars(), fields.length, sizeok);

        fields.setDim(0);   // workaround

        // Set the offsets of the fields and determine the size of the struct
        FieldState fieldState;
        bool isunion = isUnionDeclaration() !is null;
        for (size_t i = 0; i < members.length; i++)
        {
            Dsymbol s = (*members)[i];
            s.setFieldOffset(this, fieldState, isunion);
        }
        if (type.ty == Terror)
        {
            errors = true;
            return;
        }

        if (structsize == 0)
        {
            hasNoFields = true;
            alignsize = 1;

            // A fine mess of what size a zero sized struct should be
            final switch (classKind)
            {
                case ClassKind.d:
                case ClassKind.cpp:
                    structsize = 1;
                    break;

                case ClassKind.c:
                case ClassKind.objc:
                    if (target.c.bitFieldStyle == TargetC.BitFieldStyle.MS)
                    {
                        /* Undocumented MS behavior for:
                         *   struct S { int :0; };
                         */
                        structsize = 4;
                    }
                    else if (target.c.bitFieldStyle == TargetC.BitFieldStyle.DM)
                    {
                        structsize = 0;
                        alignsize = 0;
                    }
                    else
                        structsize = 0;
                    break;
            }
        }

        // Round struct size up to next alignsize boundary.
        // This will ensure that arrays of structs will get their internals
        // aligned properly.
        if (alignment.isDefault() || alignment.isPack())
            structsize = (structsize + alignsize - 1) & ~(alignsize - 1);
        else
            structsize = (structsize + alignment.get() - 1) & ~(alignment.get() - 1);

        sizeok = Sizeok.done;

        //printf("-StructDeclaration::finalizeSize() %s, fields.length = %d, structsize = %d\n", toChars(), fields.length, structsize);

        if (errors)
            return;

        // Calculate fields[i].overlapped
        if (checkOverlappedFields())
        {
            errors = true;
            return;
        }

        // Determine if struct is all zeros or not
        zeroInit = true;
        foreach (vd; fields)
        {
            if (vd._init)
            {
                if (vd._init.isVoidInitializer())
                    /* Treat as 0 for the purposes of putting the initializer
                     * in the BSS segment, or doing a mass set to 0
                     */
                    continue;

                // Zero size fields are zero initialized
                if (vd.type.size(vd.loc) == 0)
                    continue;

                // Examine init to see if it is all 0s.
                auto exp = vd.getConstInitializer();
                if (!exp || !_isZeroInit(exp))
                {
                    zeroInit = false;
                    break;
                }
            }
            else if (!vd.type.isZeroInit(loc))
            {
                zeroInit = false;
                break;
            }
        }


        argTypes = target.toArgTypes(type);
    }

    /// Compute cached type properties for `TypeStruct`
    extern(D) final void determineTypeProperties()
    {
        if (computedTypeProperties)
            return;
        foreach (vd; fields)
        {
            if (vd.storage_class & STC.ref_ || vd.hasPointers())
                hasPointerField = true;

            if (vd._init && vd._init.isVoidInitializer() && vd.type.hasPointers())
                hasVoidInitPointers = true;

            if (vd.storage_class & STC.system || vd.type.hasSystemFields())
                hasSystemFields = true;

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

        ispod = ThreeState.yes;

        if (enclosing || postblit || dtor || hasCopyCtor)
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

            Type tv = v.type.baseElemOf();
            if (tv.ty == Tstruct)
            {
                TypeStruct ts = cast(TypeStruct)tv;
                StructDeclaration sd = ts.sym;
                if (!sd.isPOD())
                {
                    ispod = ThreeState.no;
                    return false;
                }
            }
        }

        return (ispod == ThreeState.yes);
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

    override final inout(StructDeclaration) isStructDeclaration() inout @nogc nothrow pure @safe
    {
        return this;
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
     *      checkDisabled = if the struct has a regular
                            non-disabled constructor
     * Returns:
     *      true, if the struct has a regular (optionally,
     *      not disabled) constructor, false otherwise.
     */
    final bool hasRegularCtor(bool checkDisabled = false)
    {
        if (!ctor)
            return false;

        bool result;
        overloadApply(ctor, (Dsymbol s)
        {
            if (auto td = s.isTemplateDeclaration())
            {
                if (checkDisabled && td.onemember)
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
                if (!ctorDecl.isCpCtor && (!checkDisabled || !(ctorDecl.storage_class & STC.disable)))
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

/**********************************
 * Determine if exp is all binary zeros.
 * Params:
 *      exp = expression to check
 * Returns:
 *      true if it's all binary 0
 */
private bool _isZeroInit(Expression exp)
{
    switch (exp.op)
    {
        case EXP.int64:
            return exp.toInteger() == 0;

        case EXP.null_:
        case EXP.false_:
            return true;

        case EXP.structLiteral:
        {
            auto sle = cast(StructLiteralExp) exp;
            foreach (i; 0 .. sle.sd.fields.length)
            {
                auto field = sle.sd.fields[i];
                if (field.type.size(field.loc))
                {
                    auto e = (*sle.elements)[i];
                    if (e ? !_isZeroInit(e)
                          : !field.type.isZeroInit(field.loc))
                        return false;
                }
            }
            return true;
        }

        case EXP.arrayLiteral:
        {
            auto ale = cast(ArrayLiteralExp)exp;

            const dim = ale.elements ? ale.elements.length : 0;

            if (ale.type.toBasetype().ty == Tarray) // if initializing a dynamic array
                return dim == 0;

            foreach (i; 0 .. dim)
            {
                if (!_isZeroInit(ale[i]))
                    return false;
            }

            /* Note that true is returned for all T[0]
             */
            return true;
        }

        case EXP.string_:
        {
            StringExp se = cast(StringExp)exp;

            if (se.type.toBasetype().ty == Tarray) // if initializing a dynamic array
                return se.len == 0;

            foreach (i; 0 .. se.len)
            {
                if (se.getCodeUnit(i))
                    return false;
            }
            return true;
        }

        case EXP.vector:
        {
            auto ve = cast(VectorExp) exp;
            return _isZeroInit(ve.e1);
        }

        case EXP.float64:
        case EXP.complex80:
        {
            import dmd.root.ctfloat : CTFloat;
            return (exp.toReal()      is CTFloat.zero) &&
                   (exp.toImaginary() is CTFloat.zero);
        }

        default:
            return false;
    }
}

/***********************************************************
 * Unions are a variation on structs.
 */
extern (C++) final class UnionDeclaration : StructDeclaration
{
    extern (D) this(const ref Loc loc, Identifier id)
    {
        super(loc, id, false);
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

    override inout(UnionDeclaration) isUnionDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
