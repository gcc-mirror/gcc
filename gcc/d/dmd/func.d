/**
 * Defines a function declaration.
 *
 * Includes:
 * - function/delegate literals
 * - function aliases
 * - (static/shared) constructors/destructors/post-blits
 * - `invariant`
 * - `unittest`
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/func.d, _func.d)
 * Documentation:  https://dlang.org/phobos/dmd_func.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/func.d
 */

module dmd.func;

import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.blockexit;
import dmd.gluelayer;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.delegatize;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.objc;
import dmd.root.aav;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.root.stringtable;
import dmd.semantic2;
import dmd.semantic3;
import dmd.statement_rewrite_walker;
import dmd.statement;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

version (IN_GCC) {}
else version (IN_LLVM) {}
else version = MARS;

/// Inline Status
enum ILS : ubyte
{
    uninitialized,       /// not computed yet
    no,                  /// cannot inline
    yes,                 /// can inline
}

enum BUILTIN : ubyte
{
    unknown = 255,   /// not known if this is a builtin
    unimp = 0,       /// this is not a builtin
    gcc,             /// this is a GCC builtin
    llvm,            /// this is an LLVM builtin
    sin,
    cos,
    tan,
    sqrt,
    fabs,
    ldexp,
    log,
    log2,
    log10,
    exp,
    expm1,
    exp2,
    round,
    floor,
    ceil,
    trunc,
    copysign,
    pow,
    fmin,
    fmax,
    fma,
    isnan,
    isinfinity,
    isfinite,
    bsf,
    bsr,
    bswap,
    popcnt,
    yl2x,
    yl2xp1,
    toPrecFloat,
    toPrecDouble,
    toPrecReal
}

private struct FUNCFLAG
{
    bool purityInprocess;    /// working on determining purity
    bool safetyInprocess;    /// working on determining safety
    bool nothrowInprocess;   /// working on determining nothrow
    bool nogcInprocess;      /// working on determining @nogc
    bool returnInprocess;    /// working on inferring 'return' for parameters
    bool inlineScanned;      /// function has been scanned for inline possibilities
    bool inferScope;         /// infer 'scope' for parameters
    bool hasCatches;         /// function has try-catch statements
    bool skipCodegen;        /// do not generate code for this function.
    bool printf;             /// is a printf-like function

    bool scanf;              /// is a scanf-like function
    bool noreturn;           /// the function does not return
    bool isNRVO = true;      /// Support for named return value optimization
    bool isNaked;            /// The function is 'naked' (see inline ASM)
    bool isGenerated;        /// The function is compiler generated (e.g. `opCmp`)
    bool isIntroducing;      /// If this function introduces the overload set
    bool hasSemantic3Errors; /// If errors in semantic3 this function's frame ptr
    bool hasNoEH;            /// No exception unwinding is needed
    bool inferRetType;       /// Return type is to be inferred
    bool hasDualContext;     /// has a dual-context 'this' parameter

    bool hasAlwaysInlines;   /// Contains references to functions that must be inlined
    bool isCrtCtor;          /// Has attribute pragma(crt_constructor)
    bool isCrtDtor;          /// Has attribute pragma(crt_destructor)
    bool hasEscapingSiblings;/// Has sibling functions that escape
    bool computedEscapingSiblings; /// `hasEscapingSiblings` has been computed
    bool dllImport;          /// __declspec(dllimport)
    bool dllExport;          /// __declspec(dllexport)
}

/***********************************************************
 * Tuple of result identifier (possibly null) and statement.
 * This is used to store out contracts: out(id){ ensure }
 */
extern (C++) struct Ensure
{
    Identifier id;
    Statement ensure;

    Ensure syntaxCopy()
    {
        return Ensure(id, ensure.syntaxCopy());
    }

    /*****************************************
     * Do syntax copy of an array of Ensure's.
     */
    static Ensures* arraySyntaxCopy(Ensures* a)
    {
        Ensures* b = null;
        if (a)
        {
            b = a.copy();
            foreach (i, e; *a)
            {
                (*b)[i] = e.syntaxCopy();
            }
        }
        return b;
    }

}

/***********************************************************
 * Most functions don't have contracts, so save memory by grouping
 * this information into a separate struct
 */
private struct ContractInfo
{
    Statements* frequires;              /// in contracts
    Ensures* fensures;                  /// out contracts
    Statement frequire;                 /// lowered in contract
    Statement fensure;                  /// lowered out contract
    FuncDeclaration fdrequire;          /// function that does the in contract
    FuncDeclaration fdensure;           /// function that does the out contract
    Expressions* fdrequireParams;       /// argument list for __require
    Expressions* fdensureParams;        /// argument list for __ensure
}

/***********************************************************
 */
extern (C++) class FuncDeclaration : Declaration
{
    Statement fbody;                    /// function body

    FuncDeclarations foverrides;        /// functions this function overrides

    private ContractInfo* contracts;    /// contract information

    const(char)* mangleString;          /// mangled symbol created from mangleExact()

    VarDeclaration vresult;             /// result variable for out contracts
    LabelDsymbol returnLabel;           /// where the return goes

    bool[size_t] isTypeIsolatedCache;   /// cache for the potentially very expensive isTypeIsolated check

    // used to prevent symbols in different
    // scopes from having the same name
    DsymbolTable localsymtab;
    VarDeclaration vthis;               /// 'this' parameter (member and nested)
    VarDeclaration v_arguments;         /// '_arguments' parameter

    VarDeclaration v_argptr;            /// '_argptr' variable
    VarDeclarations* parameters;        /// Array of VarDeclaration's for parameters
    DsymbolTable labtab;                /// statement label symbol table
    Dsymbol overnext;                   /// next in overload list
    FuncDeclaration overnext0;          /// next in overload list (only used during IFTI)
    Loc endloc;                         /// location of closing curly bracket
    int vtblIndex = -1;                 /// for member functions, index into vtbl[]

    ILS inlineStatusStmt = ILS.uninitialized;
    ILS inlineStatusExp = ILS.uninitialized;
    PINLINE inlining = PINLINE.default_;

    int inlineNest;                     /// !=0 if nested inline

    ForeachStatement fes;               /// if foreach body, this is the foreach
    BaseClass* interfaceVirtual;        /// if virtual, but only appears in base interface vtbl[]
    /** if !=NULL, then this is the type
    of the 'introducing' function
    this one is overriding
    */
    Type tintro;

    StorageClass storage_class2;        /// storage class for template onemember's

    // Things that should really go into Scope

    /// 1 if there's a return exp; statement
    /// 2 if there's a throw statement
    /// 4 if there's an assert(0)
    /// 8 if there's inline asm
    /// 16 if there are multiple return statements
    int hasReturnExp;

    VarDeclaration nrvo_var;            /// variable to replace with shidden
    Symbol* shidden;                    /// hidden pointer passed to function

    ReturnStatements* returns;

    GotoStatements* gotos;              /// Gotos with forward references

    version (MARS)
    {
        VarDeclarations* alignSectionVars;  /// local variables with alignment needs larger than stackAlign
        Symbol* salignSection;              /// pointer to aligned section, if any
    }

    /// set if this is a known, builtin function we can evaluate at compile time
    BUILTIN builtin = BUILTIN.unknown;

    /// set if someone took the address of this function
    int tookAddressOf;

    bool requiresClosure;               // this function needs a closure

    /** local variables in this function which are referenced by nested functions
     * (They'll get put into the "closure" for this function.)
     */
    VarDeclarations closureVars;

    /** Outer variables which are referenced by this nested function
     * (the inverse of closureVars)
     */
    VarDeclarations outerVars;

    /// Sibling nested functions which called this one
    FuncDeclarations siblingCallers;

    FuncDeclarations *inlinedNestedCallees;

    /// In case of failed `@safe` inference, store the error that made the function `@system` for
    /// better diagnostics
    AttributeViolation* safetyViolation;
    AttributeViolation* nogcViolation;
    AttributeViolation* pureViolation;
    AttributeViolation* nothrowViolation;

    /// See the `FUNCFLAG` struct
    import dmd.common.bitfields;
    mixin(generateBitFields!(FUNCFLAG, uint));

    /**
     * Data for a function declaration that is needed for the Objective-C
     * integration.
     */
    ObjcFuncDeclaration objc;

    extern (D) this(const ref Loc loc, const ref Loc endloc, Identifier ident, StorageClass storage_class, Type type, bool noreturn = false)
    {
        super(loc, ident);
        //.printf("FuncDeclaration(id = '%s', type = %s)\n", ident.toChars(), type.toChars());
        //.printf("storage_class = x%llx\n", storage_class);
        this.storage_class = storage_class;
        this.type = type;
        if (type)
        {
            // Normalize storage_class, because function-type related attributes
            // are already set in the 'type' in parsing phase.
            this.storage_class &= ~(STC.TYPECTOR | STC.FUNCATTR);
        }
        this.endloc = endloc;
        if (noreturn)
            this.noreturn = true;

        /* The type given for "infer the return type" is a TypeFunction with
         * NULL for the return type.
         */
        if (type && type.nextOf() is null)
            this.inferRetType = true;
    }

    static FuncDeclaration create(const ref Loc loc, const ref Loc endloc, Identifier id, StorageClass storage_class, Type type, bool noreturn = false)
    {
        return new FuncDeclaration(loc, endloc, id, storage_class, type, noreturn);
    }

    final nothrow pure @safe
    {
        private ref ContractInfo getContracts()
        {
            if (!contracts)
                contracts = new ContractInfo();
            return *contracts;
        }

        // getters
        inout(Statements*) frequires() inout { return contracts ? contracts.frequires : null; }
        inout(Ensures*) fensures() inout { return contracts ? contracts.fensures : null; }
        inout(Statement) frequire() inout { return contracts ? contracts.frequire: null; }
        inout(Statement) fensure() inout { return contracts ? contracts.fensure : null; }
        inout(FuncDeclaration) fdrequire() inout { return contracts ? contracts.fdrequire : null; }
        inout(FuncDeclaration) fdensure() inout { return contracts ? contracts.fdensure: null; }
        inout(Expressions*) fdrequireParams() inout { return contracts ? contracts.fdrequireParams: null; }
        inout(Expressions*) fdensureParams() inout { return contracts ? contracts.fdensureParams: null; }

        extern (D) private static string generateContractSetter(string field, string type)
        {
            return type ~ " " ~ field ~ "(" ~ type ~ " param)" ~
                    "{
                        if (!param && !contracts) return null;
                        return getContracts()." ~ field ~ " = param;
                     }";
        }

        mixin(generateContractSetter("frequires", "Statements*"));
        mixin(generateContractSetter("fensures", "Ensures*"));
        mixin(generateContractSetter("frequire", "Statement"));
        mixin(generateContractSetter("fensure", "Statement"));
        mixin(generateContractSetter("fdrequire", "FuncDeclaration"));
        mixin(generateContractSetter("fdensure", "FuncDeclaration"));
        mixin(generateContractSetter("fdrequireParams", "Expressions*"));
        mixin(generateContractSetter("fdensureParams", "Expressions*"));
    }

    override FuncDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("FuncDeclaration::syntaxCopy('%s')\n", toChars());
        FuncDeclaration f = s ? cast(FuncDeclaration)s
                              : new FuncDeclaration(loc, endloc, ident, storage_class, type.syntaxCopy(), this.noreturn != 0);
        f.frequires = frequires ? Statement.arraySyntaxCopy(frequires) : null;
        f.fensures = fensures ? Ensure.arraySyntaxCopy(fensures) : null;
        f.fbody = fbody ? fbody.syntaxCopy() : null;
        return f;
    }

    override final bool equals(const RootObject o) const
    {
        if (this == o)
            return true;

        if (auto s = isDsymbol(o))
        {
            auto fd1 = this;
            auto fd2 = s.isFuncDeclaration();
            if (!fd2)
                return false;

            auto fa1 = fd1.isFuncAliasDeclaration();
            auto faf1 = fa1 ? fa1.toAliasFunc() : fd1;

            auto fa2 = fd2.isFuncAliasDeclaration();
            auto faf2 = fa2 ? fa2.toAliasFunc() : fd2;

            if (fa1 && fa2)
            {
                return faf1.equals(faf2) && fa1.hasOverloads == fa2.hasOverloads;
            }

            bool b1 = fa1 !is null;
            if (b1 && faf1.isUnique() && !fa1.hasOverloads)
                b1 = false;

            bool b2 = fa2 !is null;
            if (b2 && faf2.isUnique() && !fa2.hasOverloads)
                b2 = false;

            if (b1 != b2)
                return false;

            return faf1.toParent().equals(faf2.toParent()) &&
                   faf1.ident.equals(faf2.ident) &&
                   faf1.type.equals(faf2.type);
        }
        return false;
    }

    /****************************************************
     * Determine if 'this' overrides fd.
     * Return !=0 if it does.
     */
    extern (D) final int overrides(FuncDeclaration fd)
    {
        int result = 0;
        if (fd.ident == ident)
        {
            const cov = type.covariant(fd.type);
            if (cov != Covariant.distinct)
            {
                ClassDeclaration cd1 = toParent().isClassDeclaration();
                ClassDeclaration cd2 = fd.toParent().isClassDeclaration();
                if (cd1 && cd2 && cd2.isBaseOf(cd1, null))
                    result = 1;
            }
        }
        return result;
    }

    /****************************************************
     * Overload this FuncDeclaration with the new one f.
     * Return true if successful; i.e. no conflict.
     */
    override bool overloadInsert(Dsymbol s)
    {
        //printf("FuncDeclaration::overloadInsert(s = %s) this = %s\n", s.toChars(), toChars());
        assert(s != this);
        AliasDeclaration ad = s.isAliasDeclaration();
        if (ad)
        {
            if (overnext)
                return overnext.overloadInsert(ad);
            if (!ad.aliassym && ad.type.ty != Tident && ad.type.ty != Tinstance && ad.type.ty != Ttypeof)
            {
                //printf("\tad = '%s'\n", ad.type.toChars());
                return false;
            }
            overnext = ad;
            //printf("\ttrue: no conflict\n");
            return true;
        }
        TemplateDeclaration td = s.isTemplateDeclaration();
        if (td)
        {
            if (!td.funcroot)
                td.funcroot = this;
            if (overnext)
                return overnext.overloadInsert(td);
            overnext = td;
            return true;
        }
        FuncDeclaration fd = s.isFuncDeclaration();
        if (!fd)
            return false;

        version (none)
        {
            /* Disable this check because:
             *  const void foo();
             * semantic() isn't run yet on foo(), so the const hasn't been
             * applied yet.
             */
            if (type)
            {
                printf("type = %s\n", type.toChars());
                printf("fd.type = %s\n", fd.type.toChars());
            }
            // fd.type can be NULL for overloaded constructors
            if (type && fd.type && fd.type.covariant(type) && fd.type.mod == type.mod && !isFuncAliasDeclaration())
            {
                //printf("\tfalse: conflict %s\n", kind());
                return false;
            }
        }

        if (overnext)
        {
            td = overnext.isTemplateDeclaration();
            if (td)
                fd.overloadInsert(td);
            else
                return overnext.overloadInsert(fd);
        }
        overnext = fd;
        //printf("\ttrue: no conflict\n");
        return true;
    }

    /********************************************
     * Find function in overload list that exactly matches t.
     */
    extern (D) final FuncDeclaration overloadExactMatch(Type t)
    {
        FuncDeclaration fd;
        overloadApply(this, (Dsymbol s)
        {
            auto f = s.isFuncDeclaration();
            if (!f)
                return 0;
            if (f.storage_class & STC.disable)
                return 0;
            if (t.equals(f.type))
            {
                fd = f;
                return 1;
            }

            /* Allow covariant matches, as long as the return type
             * is just a const conversion.
             * This allows things like pure functions to match with an impure function type.
             */
            if (t.ty == Tfunction)
            {
                auto tf = cast(TypeFunction)f.type;
                if (tf.covariant(t) == Covariant.yes &&
                    tf.nextOf().implicitConvTo(t.nextOf()) >= MATCH.constant)
                {
                    fd = f;
                    return 1;
                }
            }
            return 0;
        });
        return fd;
    }

    /********************************************
     * Find function in overload list that matches to the 'this' modifier.
     * There's four result types.
     *
     * 1. If the 'tthis' matches only one candidate, it's an "exact match".
     *    Returns the function and 'hasOverloads' is set to false.
     *      eg. If 'tthis" is mutable and there's only one mutable method.
     * 2. If there's two or more match candidates, but a candidate function will be
     *    a "better match".
     *    Returns the better match function but 'hasOverloads' is set to true.
     *      eg. If 'tthis' is mutable, and there's both mutable and const methods,
     *          the mutable method will be a better match.
     * 3. If there's two or more match candidates, but there's no better match,
     *    Returns null and 'hasOverloads' is set to true to represent "ambiguous match".
     *      eg. If 'tthis' is mutable, and there's two or more mutable methods.
     * 4. If there's no candidates, it's "no match" and returns null with error report.
     *      e.g. If 'tthis' is const but there's no const methods.
     */
    extern (D) final FuncDeclaration overloadModMatch(const ref Loc loc, Type tthis, ref bool hasOverloads)
    {
        //printf("FuncDeclaration::overloadModMatch('%s')\n", toChars());
        MatchAccumulator m;
        overloadApply(this, (Dsymbol s)
        {
            auto f = s.isFuncDeclaration();
            if (!f || f == m.lastf) // skip duplicates
                return 0;

            auto tf = f.type.toTypeFunction();
            //printf("tf = %s\n", tf.toChars());

            MATCH match;
            if (tthis) // non-static functions are preferred than static ones
            {
                if (f.needThis())
                    match = f.isCtorDeclaration() ? MATCH.exact : MODmethodConv(tthis.mod, tf.mod);
                else
                    match = MATCH.constant; // keep static function in overload candidates
            }
            else // static functions are preferred than non-static ones
            {
                if (f.needThis())
                    match = MATCH.convert;
                else
                    match = MATCH.exact;
            }
            if (match == MATCH.nomatch)
                return 0;

            if (match > m.last) goto LcurrIsBetter;
            if (match < m.last) goto LlastIsBetter;

            // See if one of the matches overrides the other.
            if (m.lastf.overrides(f)) goto LlastIsBetter;
            if (f.overrides(m.lastf)) goto LcurrIsBetter;

            //printf("\tambiguous\n");
            m.nextf = f;
            m.count++;
            return 0;

        LlastIsBetter:
            //printf("\tlastbetter\n");
            m.count++; // count up
            return 0;

        LcurrIsBetter:
            //printf("\tisbetter\n");
            if (m.last <= MATCH.convert)
            {
                // clear last secondary matching
                m.nextf = null;
                m.count = 0;
            }
            m.last = match;
            m.lastf = f;
            m.count++; // count up
            return 0;
        });

        if (m.count == 1)       // exact match
        {
            hasOverloads = false;
        }
        else if (m.count > 1)   // better or ambiguous match
        {
            hasOverloads = true;
        }
        else                    // no match
        {
            hasOverloads = true;
            auto tf = this.type.toTypeFunction();
            assert(tthis);
            assert(!MODimplicitConv(tthis.mod, tf.mod)); // modifier mismatch
            {
                OutBuffer thisBuf, funcBuf;
                MODMatchToBuffer(&thisBuf, tthis.mod, tf.mod);
                MODMatchToBuffer(&funcBuf, tf.mod, tthis.mod);
                .error(loc, "%smethod %s is not callable using a %sobject", kind, toPrettyChars,
                    funcBuf.peekChars(), this.toPrettyChars(), thisBuf.peekChars());
            }
        }
        return m.lastf;
    }

    /********************************************
     * find function template root in overload list
     */
    extern (D) final TemplateDeclaration findTemplateDeclRoot()
    {
        FuncDeclaration f = this;
        while (f && f.overnext)
        {
            //printf("f.overnext = %p %s\n", f.overnext, f.overnext.toChars());
            TemplateDeclaration td = f.overnext.isTemplateDeclaration();
            if (td)
                return td;
            f = f.overnext.isFuncDeclaration();
        }
        return null;
    }

    /********************************************
     * Returns true if function was declared
     * directly or indirectly in a unittest block
     */
    final bool inUnittest()
    {
        Dsymbol f = this;
        do
        {
            if (f.isUnitTestDeclaration())
                return true;
            f = f.toParent();
        }
        while (f);
        return false;
    }

    /*************************************
     * Determine partial specialization order of functions `f` vs `g`.
     * This is very similar to TemplateDeclaration::leastAsSpecialized().
     * Params:
     *  f = first function
     *  g = second function
     *  names = names of parameters
     * Returns:
     *      match   'this' is at least as specialized as g
     *      0       g is more specialized than 'this'
     */
    static MATCH leastAsSpecialized(FuncDeclaration f, FuncDeclaration g, Identifiers* names)
    {
        enum LOG_LEASTAS = 0;
        static if (LOG_LEASTAS)
        {
            import core.stdc.stdio : printf;
            printf("leastAsSpecialized(%s, %s, %s)\n", f.toChars(), g.toChars(), names ? names.toChars() : "null");
            printf("%s, %s\n", f.type.toChars(), g.type.toChars());
        }

        /* This works by calling g() with f()'s parameters, and
         * if that is possible, then f() is at least as specialized
         * as g() is.
         */

        TypeFunction tf = f.type.toTypeFunction();
        TypeFunction tg = g.type.toTypeFunction();

        /* If both functions have a 'this' pointer, and the mods are not
         * the same and g's is not const, then this is less specialized.
         */
        if (f.needThis() && g.needThis() && tf.mod != tg.mod)
        {
            if (f.isCtorDeclaration())
            {
                if (!MODimplicitConv(tg.mod, tf.mod))
                    return MATCH.nomatch;
            }
            else
            {
                if (!MODimplicitConv(tf.mod, tg.mod))
                    return MATCH.nomatch;
            }
        }

        /* Create a dummy array of arguments out of the parameters to f()
         */
        Expressions args;
        foreach (u, p; tf.parameterList)
        {
            Expression e;
            if (p.isReference())
            {
                e = new IdentifierExp(Loc.initial, p.ident);
                e.type = p.type;
            }
            else
                e = p.type.defaultInitLiteral(Loc.initial);
            args.push(e);
        }

        MATCH m = tg.callMatch(null, ArgumentList(&args, names), 1);
        if (m > MATCH.nomatch)
        {
            /* A variadic parameter list is less specialized than a
             * non-variadic one.
             */
            if (tf.parameterList.varargs && !tg.parameterList.varargs)
                goto L1; // less specialized

            static if (LOG_LEASTAS)
            {
                printf("  matches %d, so is least as specialized\n", m);
            }
            return m;
        }
    L1:
        static if (LOG_LEASTAS)
        {
            printf("  doesn't match, so is not as specialized\n");
        }
        return MATCH.nomatch;
    }

    /********************************
     * Searches for a label with the given identifier. This function will insert a new
     * `LabelDsymbol` into `labtab` if it does not contain a mapping for `ident`.
     *
     * Params:
     *   ident = identifier of the requested label
     *   loc   = location used when creating a new `LabelDsymbol`
     *
     * Returns: the `LabelDsymbol` for `ident`
     */
    final LabelDsymbol searchLabel(Identifier ident, const ref Loc loc)
    {
        Dsymbol s;
        if (!labtab)
            labtab = new DsymbolTable(); // guess we need one

        s = labtab.lookup(ident);
        if (!s)
        {
            s = new LabelDsymbol(ident, loc);
            labtab.insert(s);
        }
        return cast(LabelDsymbol)s;
    }

    /*****************************************
     * Determine lexical level difference from `this` to nested function `fd`.
     * Params:
     *      fd = target of call
     *      intypeof = !=0 if inside typeof
     * Returns:
     *      0       same level
     *      >0      decrease nesting by number
     *      -1      increase nesting by 1 (`fd` is nested within `this`)
     *      LevelError  error, `this` cannot call `fd`
     */
    extern (D) final int getLevel(FuncDeclaration fd, int intypeof)
    {
        //printf("FuncDeclaration::getLevel(fd = '%s')\n", fd.toChars());
        Dsymbol fdparent = fd.toParent2();
        if (fdparent == this)
            return -1;

        Dsymbol s = this;
        int level = 0;
        while (fd != s && fdparent != s.toParent2())
        {
            //printf("\ts = %s, '%s'\n", s.kind(), s.toChars());
            if (auto thisfd = s.isFuncDeclaration())
            {
                if (!thisfd.isNested() && !thisfd.vthis && !intypeof)
                    return LevelError;
            }
            else
            {
                if (auto thiscd = s.isAggregateDeclaration())
                {
                    /* AggregateDeclaration::isNested returns true only when
                     * it has a hidden pointer.
                     * But, calling the function belongs unrelated lexical scope
                     * is still allowed inside typeof.
                     *
                     * struct Map(alias fun) {
                     *   typeof({ return fun(); }) RetType;
                     *   // No member function makes Map struct 'not nested'.
                     * }
                     */
                    if (!thiscd.isNested() && !intypeof)
                        return LevelError;
                }
                else
                    return LevelError;
            }

            s = s.toParentP(fd);
            assert(s);
            level++;
        }
        return level;
    }

    /***********************************
     * Determine lexical level difference from `this` to nested function `fd`.
     * Issue error if `this` cannot call `fd`.
     *
     * Params:
     *      loc = location for error messages
     *      sc = context
     *      fd = target of call
     *      decl = The `Declaration` that triggered this check.
     *             Used to provide a better error message only.
     * Returns:
     *      0       same level
     *      >0      decrease nesting by number
     *      -1      increase nesting by 1 (`fd` is nested within 'this')
     *      LevelError  error
     */
    extern (D) final int getLevelAndCheck(const ref Loc loc, Scope* sc, FuncDeclaration fd,
                                          Declaration decl)
    {
        int level = getLevel(fd, sc.intypeof);
        if (level != LevelError)
            return level;

        // Don't give error if in template constraint
        if (!(sc.flags & SCOPE.constraint))
        {
            const(char)* xstatic = isStatic() ? "`static` " : "";
            // better diagnostics for static functions
            .error(loc, "%s%s `%s` cannot access %s `%s` in frame of function `%s`",
                   xstatic, kind(), toPrettyChars(), decl.kind(), decl.toChars(),
                   fd.toPrettyChars());
                .errorSupplemental(decl.loc, "`%s` declared here", decl.toChars());
            return LevelError;
        }
        return 1;
    }

    enum LevelError = -2;

    override const(char)* toPrettyChars(bool QualifyTypes = false)
    {
        if (isMain())
            return "D main";
        else
            return Dsymbol.toPrettyChars(QualifyTypes);
    }

    /** for diagnostics, e.g. 'int foo(int x, int y) pure' */
    final const(char)* toFullSignature()
    {
        OutBuffer buf;
        functionToBufferWithIdent(type.toTypeFunction(), buf, toChars(), isStatic);
        return buf.extractChars();
    }

    final bool isMain() const
    {
        return ident == Id.main && resolvedLinkage() != LINK.c && !isMember() && !isNested();
    }

    final bool isCMain() const
    {
        return ident == Id.main && resolvedLinkage() == LINK.c && !isMember() && !isNested();
    }

    final bool isWinMain() const
    {
        //printf("FuncDeclaration::isWinMain() %s\n", toChars());
        version (none)
        {
            bool x = ident == Id.WinMain && resolvedLinkage() != LINK.c && !isMember();
            printf("%s\n", x ? "yes" : "no");
            return x;
        }
        else
        {
            return ident == Id.WinMain && resolvedLinkage() != LINK.c && !isMember();
        }
    }

    final bool isDllMain() const
    {
        return ident == Id.DllMain && resolvedLinkage() != LINK.c && !isMember();
    }

    final bool isRtInit() const
    {
        return ident == Id.rt_init && resolvedLinkage() == LINK.c && !isMember() && !isNested();
    }

    override final bool isExport() const
    {
        return visibility.kind == Visibility.Kind.export_ || dllExport;
    }

    override final bool isImportedSymbol() const
    {
        //printf("isImportedSymbol()\n");
        //printf("protection = %d\n", visibility);
        return (visibility.kind == Visibility.Kind.export_ || dllImport) && !fbody;
    }

    override final bool isCodeseg() const pure nothrow @nogc @safe
    {
        return true; // functions are always in the code segment
    }

    override final bool isOverloadable() const
    {
        return true; // functions can be overloaded
    }

    /***********************************
     * Override so it can work even if semantic() hasn't yet
     * been run.
     */
    override final bool isAbstract()
    {
        if (storage_class & STC.abstract_)
            return true;
        if (semanticRun >= PASS.semanticdone)
            return false;

        if (_scope)
        {
           if (_scope.stc & STC.abstract_)
                return true;
           parent = _scope.parent;
           Dsymbol parent = toParent();
           if (parent.isInterfaceDeclaration())
                return true;
        }
        return false;
    }

    /**********************************
     * Decide if attributes for this function can be inferred from examining
     * the function body.
     * Returns:
     *  true if can
     */
    final bool canInferAttributes(Scope* sc)
    {
        if (!fbody)
            return false;

        if (isVirtualMethod() &&
            /*
             * https://issues.dlang.org/show_bug.cgi?id=21719
             *
             * If we have an auto virtual function we can infer
             * the attributes.
             */
            !(inferRetType && !isCtorDeclaration()))
            return false;               // since they may be overridden

        if (sc.func &&
            /********** this is for backwards compatibility for the moment ********/
            (!isMember() || sc.func.isSafeBypassingInference() && !isInstantiated()))
            return true;

        if (isFuncLiteralDeclaration() ||               // externs are not possible with literals
            (storage_class & STC.inference) ||           // do attribute inference
            (inferRetType && !isCtorDeclaration()))
            return true;

        if (isInstantiated())
        {
            auto ti = parent.isTemplateInstance();
            if (ti is null || ti.isTemplateMixin() || ti.tempdecl.ident == ident)
                return true;
        }

        return false;
    }

    /*****************************************
     * Initialize for inferring the attributes of this function.
     */
    final void initInferAttributes()
    {
        //printf("initInferAttributes() for %s (%s)\n", toPrettyChars(), ident.toChars());
        TypeFunction tf = type.toTypeFunction();
        if (tf.purity == PURE.impure) // purity not specified
            purityInprocess = true;

        if (tf.trust == TRUST.default_)
            safetyInprocess = true;

        if (!tf.isnothrow)
            nothrowInprocess = true;

        if (!tf.isnogc)
            nogcInprocess = true;

        if (!isVirtual() || this.isIntroducing())
            returnInprocess = true;

        // Initialize for inferring STC.scope_
        inferScope = true;
    }

    final PURE isPure()
    {
        //printf("FuncDeclaration::isPure() '%s'\n", toChars());


        TypeFunction tf = type.toTypeFunction();
        if (purityInprocess)
            setImpure();
        if (tf.purity == PURE.fwdref)
            tf.purityLevel();
        PURE purity = tf.purity;
        if (purity > PURE.weak && isNested())
            purity = PURE.weak;
        if (purity > PURE.weak && needThis())
        {
            // The attribute of the 'this' reference affects purity strength
            if (type.mod & MODFlags.immutable_)
            {
            }
            else if (type.mod & (MODFlags.const_ | MODFlags.wild) && purity >= PURE.const_)
                purity = PURE.const_;
            else
                purity = PURE.weak;
        }
        tf.purity = purity;
        // ^ This rely on the current situation that every FuncDeclaration has a
        //   unique TypeFunction.
        return purity;
    }

    extern (D) final PURE isPureBypassingInference()
    {
        if (purityInprocess)
            return PURE.fwdref;
        else
            return isPure();
    }

    /**************************************
     * The function is doing something impure, so mark it as impure.
     *
     * Params:
     *     loc = location of impure action
     *     fmt = format string for error message. Must include "%s `%s`" for the function kind and name.
     *     arg0 = (optional) argument to format string
     *
     * Returns: `true` if there's a purity error
     */
    extern (D) final bool setImpure(Loc loc = Loc.init, const(char)* fmt = null, RootObject arg0 = null)
    {
        if (purityInprocess)
        {
            purityInprocess = false;
            if (fmt)
                pureViolation = new AttributeViolation(loc, fmt, this, arg0); // impure action
            else if (arg0)
                pureViolation = new AttributeViolation(loc, fmt, arg0); // call to impure function

            if (fes)
                fes.func.setImpure(loc, fmt, arg0);
        }
        else if (isPure())
            return true;
        return false;
    }

    extern (D) final uint flags()
    {
        return bitFields;
    }

    extern (D) final uint flags(uint f)
    {
        bitFields = f;
        return bitFields;
    }

    final bool isSafe()
    {
        if (safetyInprocess)
            setUnsafe();
        return type.toTypeFunction().trust == TRUST.safe;
    }

    extern (D) final bool isSafeBypassingInference()
    {
        return !(safetyInprocess) && isSafe();
    }

    final bool isTrusted()
    {
        if (safetyInprocess)
            setUnsafe();
        return type.toTypeFunction().trust == TRUST.trusted;
    }

    /**************************************
     * The function is doing something unsafe, so mark it as unsafe.
     *
     * Params:
     *   gag = surpress error message (used in escape.d)
     *   loc = location of error
     *   fmt = printf-style format string
     *   arg0  = (optional) argument for first %s format specifier
     *   arg1  = (optional) argument for second %s format specifier
     *   arg2  = (optional) argument for third %s format specifier
     * Returns: whether there's a safe error
     */
    extern (D) final bool setUnsafe(
        bool gag = false, Loc loc = Loc.init, const(char)* fmt = null,
        RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
    {
        if (safetyInprocess)
        {
            safetyInprocess = false;
            type.toTypeFunction().trust = TRUST.system;
            if (fmt || arg0)
                safetyViolation = new AttributeViolation(loc, fmt, arg0, arg1, arg2);

            if (fes)
                fes.func.setUnsafe();
        }
        else if (isSafe())
        {
            if (!gag && fmt)
                .error(loc, fmt, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");

            return true;
        }
        return false;
    }

    /**************************************
     * The function is calling `@system` function `f`, so mark it as unsafe.
     *
     * Params:
     *   f = function being called (needed for diagnostic of inferred functions)
     * Returns: whether there's a safe error
     */
    extern (D) final bool setUnsafeCall(FuncDeclaration f)
    {
        return setUnsafe(false, f.loc, null, f, null);
    }

    final bool isNogc()
    {
        //printf("isNogc() %s, inprocess: %d\n", toChars(), !!(flags & FUNCFLAG.nogcInprocess));
        if (nogcInprocess)
            setGC(loc, null);
        return type.toTypeFunction().isnogc;
    }

    extern (D) final bool isNogcBypassingInference()
    {
        return !nogcInprocess && isNogc();
    }

    /**************************************
     * The function is doing something that may allocate with the GC,
     * so mark it as not nogc (not no-how).
     *
     * Params:
     *     loc = location of impure action
     *     fmt = format string for error message. Must include "%s `%s`" for the function kind and name.
     *     arg0 = (optional) argument to format string
     *
     * Returns:
     *      true if function is marked as @nogc, meaning a user error occurred
     */
    extern (D) final bool setGC(Loc loc, const(char)* fmt, RootObject arg0 = null)
    {
        //printf("setGC() %s\n", toChars());
        if (nogcInprocess && semanticRun < PASS.semantic3 && _scope)
        {
            this.semantic2(_scope);
            this.semantic3(_scope);
        }

        if (nogcInprocess)
        {
            nogcInprocess = false;
            if (fmt)
                nogcViolation = new AttributeViolation(loc, fmt, this, arg0); // action that requires GC
            else if (arg0)
                nogcViolation = new AttributeViolation(loc, fmt, arg0); // call to non-@nogc function

            type.toTypeFunction().isnogc = false;
            if (fes)
                fes.func.setGC(Loc.init, null, null);
        }
        else if (isNogc())
            return true;
        return false;
    }

    /**************************************
     * The function calls non-`@nogc` function f, mark it as not nogc.
     * Params:
     *     f = function being called
     * Returns:
     *      true if function is marked as @nogc, meaning a user error occurred
     */
    extern (D) final bool setGCCall(FuncDeclaration f)
    {
        return setGC(loc, null, f);
    }

    /**************************************
     * The function is doing something that may throw an exception, register that in case nothrow is being inferred
     *
     * Params:
     *     loc = location of action
     *     fmt = format string for error message
     *     arg0 = (optional) argument to format string
     */
    extern (D) final void setThrow(Loc loc, const(char)* fmt, RootObject arg0 = null)
    {
        if (nothrowInprocess && !nothrowViolation)
        {
            nothrowViolation = new AttributeViolation(loc, fmt, arg0); // action that requires GC
        }
    }

    /**************************************
     * The function calls non-`nothrow` function f, register that in case nothrow is being inferred
     * Params:
     *     loc = location of call
     *     f = function being called
     */
    extern (D) final void setThrowCall(Loc loc, FuncDeclaration f)
    {
        return setThrow(loc, null, f);
    }

    extern (D) final void printGCUsage(const ref Loc loc, const(char)* warn)
    {
        if (!global.params.v.gc)
            return;

        Module m = getModule();
        if (m && m.isRoot() && !inUnittest())
        {
            message(loc, "vgc: %s", warn);
        }
    }

    /********************************************
     * See if pointers from function parameters, mutable globals, or uplevel functions
     * could leak into return value.
     * Returns:
     *   true if the function return value is isolated from
     *   any inputs to the function
     */
    extern (D) final bool isReturnIsolated()
    {
        //printf("isReturnIsolated(this: %s)\n", this.toChars);
        TypeFunction tf = type.toTypeFunction();
        assert(tf.next);

        Type treti = tf.next;
        if (tf.isref)
            return isTypeIsolatedIndirect(treti);              // check influence from parameters

        return isTypeIsolated(treti);
    }

    /********************
     * See if pointers from function parameters, mutable globals, or uplevel functions
     * could leak into type `t`.
     * Params:
     *   t = type to check if it is isolated
     * Returns:
     *   true if `t` is isolated from
     *   any inputs to the function
     */
    extern (D) final bool isTypeIsolated(Type t)
    {
        StringTable!Type parentTypes;
        const uniqueTypeID = t.getUniqueID();
        if (uniqueTypeID)
        {
            const cacheResultPtr = uniqueTypeID in isTypeIsolatedCache;
            if (cacheResultPtr !is null)
                return *cacheResultPtr;

            parentTypes._init();
            const isIsolated = isTypeIsolated(t, parentTypes);
            isTypeIsolatedCache[uniqueTypeID] = isIsolated;
            return isIsolated;
        }
        else
        {
            parentTypes._init();
            return isTypeIsolated(t, parentTypes);
        }
    }

    ///ditto
    extern (D) final bool isTypeIsolated(Type t, ref StringTable!Type parentTypes)
    {
        //printf("this: %s, isTypeIsolated(t: %s)\n", this.toChars(), t.toChars());

        t = t.baseElemOf();
        switch (t.ty)
        {
            case Tarray:
            case Tpointer:
                return isTypeIsolatedIndirect(t.nextOf()); // go down one level

            case Taarray:
            case Tclass:
                return isTypeIsolatedIndirect(t);

            case Tstruct:
                /* Drill down and check the struct's fields
                 */
                auto sym = t.toDsymbol(null).isStructDeclaration();
                const tName = t.toChars.toDString;
                const entry = parentTypes.insert(tName, t);
                if (entry == null)
                {
                    //we've already seen this type in a parent, not isolated
                    return false;
                }
                foreach (v; sym.fields)
                {
                    Type tmi = v.type.addMod(t.mod);
                    //printf("\tt = %s, v: %s, vtype: %s,  tmi = %s\n",
                    //       t.toChars(), v.toChars(), v.type.toChars(), tmi.toChars());
                    if (!isTypeIsolated(tmi, parentTypes))
                        return false;
                }
                return true;

            default:
                return true;
        }
    }

    /********************************************
     * Params:
     *    t = type of object to test one level of indirection down
     * Returns:
     *    true if an object typed `t` has no indirections
     *    which could have come from the function's parameters, mutable
     *    globals, or uplevel functions.
     */
    private bool isTypeIsolatedIndirect(Type t)
    {
        //printf("isTypeIsolatedIndirect(t: %s)\n", t.toChars());
        assert(t);

        /* Since `t` is one level down from an indirection, it could pick
         * up a reference to a mutable global or an outer function, so
         * return false.
         */
        if (!isPureBypassingInference() || isNested())
            return false;

        TypeFunction tf = type.toTypeFunction();

        //printf("isTypeIsolatedIndirect(%s) t = %s\n", tf.toChars(), t.toChars());

        foreach (i, fparam; tf.parameterList)
        {
            Type tp = fparam.type;
            if (!tp)
                continue;

            if (fparam.isLazy() || fparam.isReference())
            {
                if (!traverseIndirections(tp, t))
                    return false;
                continue;
            }

            /* Goes down one level of indirection, then calls traverseIndirection() on
             * the result.
             * Returns:
             *  true if t is isolated from tp
             */
            static bool traverse(Type tp, Type t)
            {
                tp = tp.baseElemOf();
                switch (tp.ty)
                {
                    case Tarray:
                    case Tpointer:
                        return traverseIndirections(tp.nextOf(), t);

                    case Taarray:
                    case Tclass:
                        return traverseIndirections(tp, t);

                    case Tstruct:
                        /* Drill down and check the struct's fields
                         */
                        auto sym = tp.toDsymbol(null).isStructDeclaration();
                        foreach (v; sym.fields)
                        {
                            Type tprmi = v.type.addMod(tp.mod);
                            //printf("\ttp = %s, tprmi = %s\n", tp.toChars(), tprmi.toChars());
                            if (!traverse(tprmi, t))
                                return false;
                        }
                        return true;

                    default:
                        return true;
                }
            }

            if (!traverse(tp, t))
                return false;
        }
        // The 'this' reference is a parameter, too
        if (AggregateDeclaration ad = isCtorDeclaration() ? null : isThis())
        {
            Type tthis = ad.getType().addMod(tf.mod);
            //printf("\ttthis = %s\n", tthis.toChars());
            if (!traverseIndirections(tthis, t))
                return false;
        }

        return true;
    }

    /****************************************
     * Determine if function needs a static frame pointer.
     * Returns:
     *  `true` if function is really nested within other function.
     * Contracts:
     *  If isNested() returns true, isThis() should return false,
     *  unless the function needs a dual-context pointer.
     */
    bool isNested() const
    {
        auto f = toAliasFunc();
        //printf("\ttoParent2() = '%s'\n", f.toParent2().toChars());
        return ((f.storage_class & STC.static_) == 0) &&
                (f._linkage == LINK.d) &&
                (f.toParent2().isFuncDeclaration() !is null ||
                 f.toParent2() !is f.toParentLocal());
    }

    /****************************************
     * Determine if function is a non-static member function
     * that has an implicit 'this' expression.
     * Returns:
     *  The aggregate it is a member of, or null.
     * Contracts:
     *  Both isThis() and isNested() should return true if function needs a dual-context pointer,
     *  otherwise if isThis() returns true, isNested() should return false.
     */
    override inout(AggregateDeclaration) isThis() inout
    {
        //printf("+FuncDeclaration::isThis() '%s'\n", toChars());
        auto ad = (storage_class & STC.static_) ? .objc.isThis(this) : isMemberLocal();
        //printf("-FuncDeclaration::isThis() %p\n", ad);
        return ad;
    }

    override final bool needThis()
    {
        //printf("FuncDeclaration::needThis() '%s'\n", toChars());
        return toAliasFunc().isThis() !is null;
    }

    // Determine if a function is pedantically virtual
    final bool isVirtualMethod()
    {
        if (toAliasFunc() != this)
            return toAliasFunc().isVirtualMethod();

        //printf("FuncDeclaration::isVirtualMethod() %s\n", toChars());
        if (!isVirtual())
            return false;
        // If it's a final method, and does not override anything, then it is not virtual
        if (isFinalFunc() && foverrides.length == 0)
        {
            return false;
        }
        return true;
    }

    // Determine if function goes into virtual function pointer table
    bool isVirtual() const
    {
        if (toAliasFunc() != this)
            return toAliasFunc().isVirtual();

        auto p = toParent();

        if (!isMember || !p.isClassDeclaration)
            return false;

        if (p.isClassDeclaration.classKind == ClassKind.objc)
            return .objc.isVirtual(this);

        version (none)
        {
            printf("FuncDeclaration::isVirtual(%s)\n", toChars());
            printf("isMember:%p isStatic:%d private:%d ctor:%d !Dlinkage:%d\n", isMember(), isStatic(), visibility == Visibility.Kind.private_, isCtorDeclaration(), linkage != LINK.d);
            printf("result is %d\n", isMember() && !(isStatic() || visibility == Visibility.Kind.private_ || visibility == Visibility.Kind.package_) && p.isClassDeclaration() && !(p.isInterfaceDeclaration() && isFinalFunc()));
        }
        return !(isStatic() || visibility.kind == Visibility.Kind.private_ || visibility.kind == Visibility.Kind.package_) && !(p.isInterfaceDeclaration() && isFinalFunc());
    }

    final bool isFinalFunc() const
    {
        if (toAliasFunc() != this)
            return toAliasFunc().isFinalFunc();

        version (none)
        {{
            auto cd = toParent().isClassDeclaration();
            printf("FuncDeclaration::isFinalFunc(%s), %x\n", toChars(), Declaration.isFinal());
            printf("%p %d %d %d\n", isMember(), isStatic(), Declaration.isFinal(), ((cd = toParent().isClassDeclaration()) !is null && cd.storage_class & STC.final_));
            printf("result is %d\n", isMember() && (Declaration.isFinal() || (cd !is null && cd.storage_class & STC.final_)));
            if (cd)
                printf("\tmember of %s\n", cd.toChars());
        }}
        if (!isMember())
            return false;
        if (Declaration.isFinal())
            return true;
        auto cd = toParent().isClassDeclaration();
        return (cd !is null) && (cd.storage_class & STC.final_);
    }

    bool addPreInvariant()
    {
        auto ad = isThis();
        ClassDeclaration cd = ad ? ad.isClassDeclaration() : null;
        return (ad && !(cd && cd.isCPPclass()) && global.params.useInvariants == CHECKENABLE.on && (visibility.kind == Visibility.Kind.protected_ || visibility.kind == Visibility.Kind.public_ || visibility.kind == Visibility.Kind.export_) && !this.isNaked());
    }

    bool addPostInvariant()
    {
        auto ad = isThis();
        ClassDeclaration cd = ad ? ad.isClassDeclaration() : null;
        return (ad && !(cd && cd.isCPPclass()) && ad.inv && global.params.useInvariants == CHECKENABLE.on && (visibility.kind == Visibility.Kind.protected_ || visibility.kind == Visibility.Kind.public_ || visibility.kind == Visibility.Kind.export_) && !this.isNaked());
    }

    override const(char)* kind() const
    {
        return this.isGenerated() ? "generated function" : "function";
    }

    /********************************************
     * Returns:
     *  true if there are no overloads of this function
     */
    final bool isUnique() const
    {
        bool result = false;
        overloadApply(cast() this, (Dsymbol s)
        {
            auto f = s.isFuncDeclaration();
            auto td = s.isTemplateDeclaration();
            if (!f && !td)
                return 0;
            if (result)
            {
                result = false;
                return 1; // ambiguous, done
            }
            else
            {
                result = true;
                return 0;
            }
        });
        return result;
    }

    /*********************************************
     * In the current function, we are calling 'this' function.
     * 1. Check to see if the current function can call 'this' function, issue error if not.
     * 2. If the current function is not the parent of 'this' function, then add
     *    the current function to the list of siblings of 'this' function.
     * 3. If the current function is a literal, and it's accessing an uplevel scope,
     *    then mark it as a delegate.
     * Returns true if error occurs.
     */
    extern (D) final bool checkNestedReference(Scope* sc, const ref Loc loc)
    {
        //printf("FuncDeclaration::checkNestedReference() %s\n", toPrettyChars());

        if (auto fld = this.isFuncLiteralDeclaration())
        {
            if (fld.tok == TOK.reserved)
            {
                fld.tok = TOK.function_;
                fld.vthis = null;
            }
        }

        if (!parent || parent == sc.parent)
            return false;
        if (ident == Id.require || ident == Id.ensure)
            return false;
        if (!isThis() && !isNested())
            return false;

        // The current function
        FuncDeclaration fdthis = sc.parent.isFuncDeclaration();
        if (!fdthis)
            return false; // out of function scope

        Dsymbol p = toParentLocal();
        Dsymbol p2 = toParent2();

        // Function literals from fdthis to p must be delegates
        ensureStaticLinkTo(fdthis, p);
        if (p != p2)
            ensureStaticLinkTo(fdthis, p2);

        if (isNested())
        {
            // The function that this function is in
            bool checkEnclosing(FuncDeclaration fdv)
            {
                if (!fdv)
                    return false;
                if (fdv == fdthis)
                    return false;

                //printf("this = %s in [%s]\n", this.toChars(), this.loc.toChars());
                //printf("fdv  = %s in [%s]\n", fdv .toChars(), fdv .loc.toChars());
                //printf("fdthis = %s in [%s]\n", fdthis.toChars(), fdthis.loc.toChars());

                // Add this function to the list of those which called us
                if (fdthis != this)
                {
                    bool found = false;
                    for (size_t i = 0; i < siblingCallers.length; ++i)
                    {
                        if (siblingCallers[i] == fdthis)
                            found = true;
                    }
                    if (!found)
                    {
                        //printf("\tadding sibling %s to %s\n", fdthis.toPrettyChars(), toPrettyChars());
                        if (!sc.intypeof && !(sc.flags & SCOPE.compile))
                        {
                            siblingCallers.push(fdthis);
                            computedEscapingSiblings = false;
                        }
                    }
                }

                const lv = fdthis.getLevelAndCheck(loc, sc, fdv, this);
                if (lv == LevelError)
                    return true; // error
                if (lv == -1)
                    return false; // downlevel call
                if (lv == 0)
                    return false; // same level call

                return false; // Uplevel call
            }

            if (checkEnclosing(p.isFuncDeclaration()))
                return true;
            if (checkEnclosing(p == p2 ? null : p2.isFuncDeclaration()))
                return true;
        }
        return false;
    }

    /*******************************
     * Look at all the variables in this function that are referenced
     * by nested functions, and determine if a closure needs to be
     * created for them.
     */
    final bool needsClosure()
    {
        /* Need a closure for all the closureVars[] if any of the
         * closureVars[] are accessed by a
         * function that escapes the scope of this function.
         * We take the conservative approach and decide that a function needs
         * a closure if it:
         * 1) is a virtual function
         * 2) has its address taken
         * 3) has a parent that escapes
         * 4) calls another nested function that needs a closure
         *
         * Note that since a non-virtual function can be called by
         * a virtual one, if that non-virtual function accesses a closure
         * var, the closure still has to be taken. Hence, we check for isThis()
         * instead of isVirtual(). (thanks to David Friedman)
         *
         * When the function returns a local struct or class, `requiresClosure`
         * is already set to `true` upon entering this function when the
         * struct/class refers to a local variable and a closure is needed.
         */
        //printf("FuncDeclaration::needsClosure() %s\n", toPrettyChars());

        if (requiresClosure)
            goto Lyes;

        for (size_t i = 0; i < closureVars.length; i++)
        {
            VarDeclaration v = closureVars[i];
            //printf("\tv = %s\n", v.toChars());

            for (size_t j = 0; j < v.nestedrefs.length; j++)
            {
                FuncDeclaration f = v.nestedrefs[j];
                assert(f != this);

                /* __require and __ensure will always get called directly,
                 * so they never make outer functions closure.
                 */
                if (f.ident == Id.require || f.ident == Id.ensure)
                    continue;

                //printf("\t\tf = %p, %s, isVirtual=%d, isThis=%p, tookAddressOf=%d\n", f, f.toChars(), f.isVirtual(), f.isThis(), f.tookAddressOf);

                /* Look to see if f escapes. We consider all parents of f within
                 * this, and also all siblings which call f; if any of them escape,
                 * so does f.
                 * Mark all affected functions as requiring closures.
                 */
                for (Dsymbol s = f; s && s != this; s = s.toParentP(this))
                {
                    FuncDeclaration fx = s.isFuncDeclaration();
                    if (!fx)
                        continue;
                    if (fx.isThis() || fx.tookAddressOf)
                    {
                        //printf("\t\tfx = %s, isVirtual=%d, isThis=%p, tookAddressOf=%d\n", fx.toChars(), fx.isVirtual(), fx.isThis(), fx.tookAddressOf);

                        /* Mark as needing closure any functions between this and f
                         */
                        markAsNeedingClosure((fx == f) ? fx.toParentP(this) : fx, this);

                        requiresClosure = true;
                    }

                    /* We also need to check if any sibling functions that
                     * called us, have escaped. This is recursive: we need
                     * to check the callers of our siblings.
                     */
                    if (checkEscapingSiblings(fx, this))
                        requiresClosure = true;

                    /* https://issues.dlang.org/show_bug.cgi?id=12406
                     * Iterate all closureVars to mark all descendant
                     * nested functions that access to the closing context of this function.
                     */
                }
            }
        }
        if (requiresClosure)
            goto Lyes;

        return false;

    Lyes:
        return true;
    }

    /***********************************************
     * Check that the function contains any closure.
     * If it's @nogc, report suitable errors.
     * This is mostly consistent with FuncDeclaration::needsClosure().
     *
     * Returns:
     *      true if any errors occur.
     */
    extern (C++) final bool checkClosure()
    {
        //printf("checkClosure() %s\n", toPrettyChars());
        if (!needsClosure())
            return false;

        if (setGC(loc, "%s `%s` is `@nogc` yet allocates closure for `%s()` with the GC", this))
        {
            .error(loc, "%s `%s` is `@nogc` yet allocates closure for `%s()` with the GC", kind, toPrettyChars, toChars());
            if (global.gag)     // need not report supplemental errors
                return true;
        }
        else if (!global.params.useGC)
        {
            .error(loc, "%s `%s` is `-betterC` yet allocates closure for `%s()` with the GC", kind, toPrettyChars, toChars());
            if (global.gag)     // need not report supplemental errors
                return true;
        }
        else
        {
            printGCUsage(loc, "using closure causes GC allocation");
            return false;
        }

        FuncDeclarations a;
        foreach (v; closureVars)
        {
            foreach (f; v.nestedrefs)
            {
                assert(f !is this);

            LcheckAncestorsOfANestedRef:
                for (Dsymbol s = f; s && s !is this; s = s.toParentP(this))
                {
                    auto fx = s.isFuncDeclaration();
                    if (!fx)
                        continue;
                    if (fx.isThis() ||
                        fx.tookAddressOf ||
                        checkEscapingSiblings(fx, this))
                    {
                        foreach (f2; a)
                        {
                            if (f2 == f)
                                break LcheckAncestorsOfANestedRef;
                        }
                        a.push(f);
                        .errorSupplemental(f.loc, "%s `%s` closes over variable `%s`",
                            f.kind, f.toPrettyChars(), v.toChars());
                        if (v.ident != Id.This)
                            .errorSupplemental(v.loc, "`%s` declared here", v.toChars());

                        break LcheckAncestorsOfANestedRef;
                    }
                }
            }
        }

        return true;
    }

    /***********************************************
     * Determine if function's variables are referenced by a function
     * nested within it.
     */
    final bool hasNestedFrameRefs()
    {
        if (closureVars.length)
            return true;

        /* If a virtual function has contracts, assume its variables are referenced
         * by those contracts, even if they aren't. Because they might be referenced
         * by the overridden or overriding function's contracts.
         * This can happen because frequire and fensure are implemented as nested functions,
         * and they can be called directly by an overriding function and the overriding function's
         * context had better match, or
         * https://issues.dlang.org/show_bug.cgi?id=7335 will bite.
         */
        if (fdrequire || fdensure)
            return true;

        if (foverrides.length && isVirtualMethod())
        {
            for (size_t i = 0; i < foverrides.length; i++)
            {
                FuncDeclaration fdv = foverrides[i];
                if (fdv.hasNestedFrameRefs())
                    return true;
            }
        }
        return false;
    }

    /****************************************************
     * Check whether result variable can be built.
     * Returns:
     *     `true` if the function has a return type that
     *     is different from `void`.
     */
    extern (D) private bool canBuildResultVar()
    {
        auto f = cast(TypeFunction)type;
        return f && f.nextOf() && f.nextOf().toBasetype().ty != Tvoid;
    }

    /****************************************************
     * Merge into this function the 'in' contracts of all it overrides.
     * 'in's are OR'd together, i.e. only one of them needs to pass.
     */
    extern (D) final Statement mergeFrequire(Statement sf, Expressions* params)
    {
        /* If a base function and its override both have an IN contract, then
         * only one of them needs to succeed. This is done by generating:
         *
         * void derived.in() {
         *  try {
         *    base.in();
         *  }
         *  catch () {
         *    ... body of derived.in() ...
         *  }
         * }
         *
         * So if base.in() doesn't throw, derived.in() need not be executed, and the contract is valid.
         * If base.in() throws, then derived.in()'s body is executed.
         */

        foreach (fdv; foverrides)
        {
            /* The semantic pass on the contracts of the overridden functions must
             * be completed before code generation occurs.
             * https://issues.dlang.org/show_bug.cgi?id=3602
             */
            if (fdv.frequires && fdv.semanticRun != PASS.semantic3done)
            {
                assert(fdv._scope);
                Scope* sc = fdv._scope.push();
                sc.stc &= ~STC.override_;
                fdv.semantic3(sc);
                sc.pop();
            }

            sf = fdv.mergeFrequire(sf, params);
            if (!sf || !fdv.fdrequire)
                return null;
            //printf("fdv.frequire: %s\n", fdv.frequire.toChars());
            /* Make the call:
                *   try { __require(params); }
                *   catch (Throwable) { frequire; }
                */
            params = Expression.arraySyntaxCopy(params);
            Expression e = new CallExp(loc, new VarExp(loc, fdv.fdrequire, false), params);
            Statement s2 = new ExpStatement(loc, e);

            auto c = new Catch(loc, getThrowable(), null, sf);
            c.internalCatch = true;
            auto catches = new Catches();
            catches.push(c);
            sf = new TryCatchStatement(loc, s2, catches);
        }
        return sf;
    }

    /****************************************************
     * Merge into this function the 'in' contracts of all it overrides.
     */
    extern (D) final Statement mergeFrequireInclusivePreview(Statement sf, Expressions* params)
    {
        /* If a base function and its override both have an IN contract, then
         * the override in contract must widen the guarantee of the base contract.
         * This is checked by generating:
         *
         * void derived.in() {
         *  try {
         *    ... body of derived.in() ...
         *  }
         *  catch () {
         *    // derived in rejected this argument. so parent must also reject it, or we've tightened the contract.
         *    base.in();
         *    assert(false, "Logic error: " ~ thr.msg);
         *  }
         * }
         */

        foreach (fdv; foverrides)
        {
            /* The semantic pass on the contracts of the overridden functions must
             * be completed before code generation occurs.
             * https://issues.dlang.org/show_bug.cgi?id=3602
             */
            if (fdv.frequires && fdv.semanticRun != PASS.semantic3done)
            {
                assert(fdv._scope);
                Scope* sc = fdv._scope.push();
                sc.stc &= ~STC.override_;
                fdv.semantic3(sc);
                sc.pop();
            }

            sf = fdv.mergeFrequireInclusivePreview(sf, params);
            if (sf && fdv.fdrequire)
            {
                const loc = this.fdrequire.loc;

                //printf("fdv.frequire: %s\n", fdv.frequire.toChars());
                /* Make the call:
                 *   try { frequire; }
                 *   catch (Throwable thr) { __require(params); assert(false, "Logic error: " ~ thr.msg); }
                 */
                Identifier id = Identifier.generateId("thr");
                params = Expression.arraySyntaxCopy(params);
                Expression e = new CallExp(loc, new VarExp(loc, fdv.fdrequire, false), params);
                Statement s2 = new ExpStatement(loc, e);
                // assert(false, ...)
                // TODO make this a runtime helper to allow:
                // - chaining the original expression
                // - nogc concatenation
                Expression msg = new StringExp(loc, "Logic error: in-contract was tighter than parent in-contract");
                Statement fail = new ExpStatement(loc, new AssertExp(loc, IntegerExp.literal!0, msg));

                Statement s3 = new CompoundStatement(loc, s2, fail);

                auto c = new Catch(loc, getThrowable(), id, s3);
                c.internalCatch = true;
                auto catches = new Catches();
                catches.push(c);
                sf = new TryCatchStatement(loc, sf, catches);
            }
            else
                return null;
        }
        return sf;
    }

    /****************************************************
     * Determine whether an 'out' contract is declared inside
     * the given function or any of its overrides.
     * Params:
     *      fd = the function to search
     * Returns:
     *      true    found an 'out' contract
     */
    static bool needsFensure(FuncDeclaration fd) @safe
    {
        if (fd.fensures)
            return true;

        foreach (fdv; fd.foverrides)
        {
            if (needsFensure(fdv))
                return true;
        }
        return false;
    }

    /****************************************************
     * Rewrite contracts as statements.
     */
    final void buildEnsureRequire()
    {

        if (frequires)
        {
            /*   in { statements1... }
             *   in { statements2... }
             *   ...
             * becomes:
             *   in { { statements1... } { statements2... } ... }
             */
            assert(frequires.length);
            auto loc = (*frequires)[0].loc;
            auto s = new Statements;
            foreach (r; *frequires)
            {
                s.push(new ScopeStatement(r.loc, r, r.loc));
            }
            frequire = new CompoundStatement(loc, s);
        }

        if (fensures)
        {
            /*   out(id1) { statements1... }
             *   out(id2) { statements2... }
             *   ...
             * becomes:
             *   out(__result) { { ref id1 = __result; { statements1... } }
             *                   { ref id2 = __result; { statements2... } } ... }
             */
            assert(fensures.length);
            auto loc = (*fensures)[0].ensure.loc;
            auto s = new Statements;
            foreach (r; *fensures)
            {
                if (r.id && canBuildResultVar())
                {
                    auto rloc = r.ensure.loc;
                    auto resultId = new IdentifierExp(rloc, Id.result);
                    auto init = new ExpInitializer(rloc, resultId);
                    auto stc = STC.ref_ | STC.temp | STC.result;
                    auto decl = new VarDeclaration(rloc, null, r.id, init, stc);
                    auto sdecl = new ExpStatement(rloc, decl);
                    s.push(new ScopeStatement(rloc, new CompoundStatement(rloc, sdecl, r.ensure), rloc));
                }
                else
                {
                    s.push(r.ensure);
                }
            }
            fensure = new CompoundStatement(loc, s);
        }

        if (!isVirtual())
            return;

        /* Rewrite contracts as nested functions, then call them. Doing it as nested
         * functions means that overriding functions can call them.
         */
        TypeFunction f = cast(TypeFunction) type;

        /* Make a copy of the parameters and make them all ref */
        static Parameters* toRefCopy(ParameterList parameterList)
        {
            auto result = new Parameters();

            foreach (n, p; parameterList)
            {
                p = p.syntaxCopy();
                if (!p.isLazy())
                    p.storageClass = (p.storageClass | STC.ref_) & ~STC.out_;
                p.defaultArg = null; // won't be the same with ref
                result.push(p);
            }

            return result;
        }

        if (frequire)
        {
            /*   in { ... }
             * becomes:
             *   void __require(ref params) { ... }
             *   __require(params);
             */
            Loc loc = frequire.loc;
            fdrequireParams = new Expressions();
            if (parameters)
            {
                foreach (vd; *parameters)
                    fdrequireParams.push(new VarExp(loc, vd));
            }
            auto fo = cast(TypeFunction)(originalType ? originalType : f);
            auto fparams = toRefCopy(fo.parameterList);
            auto tf = new TypeFunction(ParameterList(fparams), Type.tvoid, LINK.d);
            tf.isnothrow = f.isnothrow;
            tf.isnogc = f.isnogc;
            tf.purity = f.purity;
            tf.trust = f.trust;
            auto fd = new FuncDeclaration(loc, loc, Id.require, STC.undefined_, tf);
            fd.fbody = frequire;
            Statement s1 = new ExpStatement(loc, fd);
            Expression e = new CallExp(loc, new VarExp(loc, fd, false), fdrequireParams);
            Statement s2 = new ExpStatement(loc, e);
            frequire = new CompoundStatement(loc, s1, s2);
            fdrequire = fd;
        }

        /* We need to set fdensureParams here and not in the block below to
         * have the parameters available when calling a base class ensure(),
         * even if this function doesn't have an out contract.
         */
        fdensureParams = new Expressions();
        if (canBuildResultVar())
            fdensureParams.push(new IdentifierExp(loc, Id.result));
        if (parameters)
        {
            foreach (vd; *parameters)
                fdensureParams.push(new VarExp(loc, vd));
        }

        if (fensure)
        {
            /*   out (result) { ... }
             * becomes:
             *   void __ensure(ref tret result, ref params) { ... }
             *   __ensure(result, params);
             */
            Loc loc = fensure.loc;
            auto fparams = new Parameters();
            if (canBuildResultVar())
            {
                Parameter p = new Parameter(loc, STC.ref_ | STC.const_, f.nextOf(), Id.result, null, null);
                fparams.push(p);
            }
            auto fo = cast(TypeFunction)(originalType ? originalType : f);
            fparams.pushSlice((*toRefCopy(fo.parameterList))[]);
            auto tf = new TypeFunction(ParameterList(fparams), Type.tvoid, LINK.d);
            tf.isnothrow = f.isnothrow;
            tf.isnogc = f.isnogc;
            tf.purity = f.purity;
            tf.trust = f.trust;
            auto fd = new FuncDeclaration(loc, loc, Id.ensure, STC.undefined_, tf);
            fd.fbody = fensure;
            Statement s1 = new ExpStatement(loc, fd);
            Expression e = new CallExp(loc, new VarExp(loc, fd, false), fdensureParams);
            Statement s2 = new ExpStatement(loc, e);
            fensure = new CompoundStatement(loc, s1, s2);
            fdensure = fd;
        }
    }

    /****************************************************
     * Merge into this function the 'out' contracts of all it overrides.
     * 'out's are AND'd together, i.e. all of them need to pass.
     */
    extern (D) final Statement mergeFensure(Statement sf, Identifier oid, Expressions* params)
    {
        /* Same comments as for mergeFrequire(), except that we take care
         * of generating a consistent reference to the 'result' local by
         * explicitly passing 'result' to the nested function as a reference
         * argument.
         * This won't work for the 'this' parameter as it would require changing
         * the semantic code for the nested function so that it looks on the parameter
         * list for the 'this' pointer, something that would need an unknown amount
         * of tweaking of various parts of the compiler that I'd rather leave alone.
         */
        foreach (fdv; foverrides)
        {
            /* The semantic pass on the contracts of the overridden functions must
             * be completed before code generation occurs.
             * https://issues.dlang.org/show_bug.cgi?id=3602 and
             * https://issues.dlang.org/show_bug.cgi?id=5230
             */
            if (needsFensure(fdv) && fdv.semanticRun != PASS.semantic3done)
            {
                assert(fdv._scope);
                Scope* sc = fdv._scope.push();
                sc.stc &= ~STC.override_;
                fdv.semantic3(sc);
                sc.pop();
            }

            sf = fdv.mergeFensure(sf, oid, params);
            if (fdv.fdensure)
            {
                //printf("fdv.fensure: %s\n", fdv.fensure.toChars());
                // Make the call: __ensure(result, params)
                params = Expression.arraySyntaxCopy(params);
                if (canBuildResultVar())
                {
                    Type t1 = fdv.type.nextOf().toBasetype();
                    Type t2 = this.type.nextOf().toBasetype();
                    if (t1.isBaseOf(t2, null))
                    {
                        /* Making temporary reference variable is necessary
                         * in covariant return.
                         * https://issues.dlang.org/show_bug.cgi?id=5204
                         * https://issues.dlang.org/show_bug.cgi?id=10479
                         */
                        Expression* eresult = &(*params)[0];
                        auto ei = new ExpInitializer(Loc.initial, *eresult);
                        auto v = new VarDeclaration(Loc.initial, t1, Identifier.generateId("__covres"), ei);
                        v.storage_class |= STC.temp;
                        auto de = new DeclarationExp(Loc.initial, v);
                        auto ve = new VarExp(Loc.initial, v);
                        *eresult = new CommaExp(Loc.initial, de, ve);
                    }
                }
                Expression e = new CallExp(loc, new VarExp(loc, fdv.fdensure, false), params);
                Statement s2 = new ExpStatement(loc, e);

                if (sf)
                {
                    sf = new CompoundStatement(sf.loc, s2, sf);
                }
                else
                    sf = s2;
            }
        }
        return sf;
    }

    /*********************************************
     * Returns: the function's parameter list, and whether
     * it is variadic or not.
     */
    final ParameterList getParameterList()
    {
        if (type)
        {
            TypeFunction fdtype = type.isTypeFunction();
            if (fdtype) // Could also be TypeError
                return fdtype.parameterList;
        }

        return ParameterList(null, VarArg.none);
    }

    /**********************************
     * Generate a FuncDeclaration for a runtime library function.
     */
    static FuncDeclaration genCfunc(Parameters* fparams, Type treturn, const(char)* name, StorageClass stc = 0)
    {
        return genCfunc(fparams, treturn, Identifier.idPool(name[0 .. strlen(name)]), stc);
    }

    static FuncDeclaration genCfunc(Parameters* fparams, Type treturn, Identifier id, StorageClass stc = 0)
    {
        FuncDeclaration fd;
        TypeFunction tf;
        Dsymbol s;
        __gshared DsymbolTable st = null;

        //printf("genCfunc(name = '%s')\n", id.toChars());
        //printf("treturn\n\t"); treturn.print();

        // See if already in table
        if (!st)
            st = new DsymbolTable();
        s = st.lookup(id);
        if (s)
        {
            fd = s.isFuncDeclaration();
            assert(fd);
            assert(fd.type.nextOf().equals(treturn));
        }
        else
        {
            tf = new TypeFunction(ParameterList(fparams), treturn, LINK.c, stc);
            fd = new FuncDeclaration(Loc.initial, Loc.initial, id, STC.static_, tf);
            fd.visibility = Visibility(Visibility.Kind.public_);
            fd._linkage = LINK.c;

            st.insert(fd);
        }
        return fd;
    }

    /+
     + Checks the parameter and return types iff this is a `main` function.
     +
     + The following signatures are allowed for a `D main`:
     + - Either no or a single parameter of type `string[]`
     + - Return type is either `void`, `int` or `noreturn`
     +
     + The following signatures are standard C:
     + - `int main()`
     + - `int main(int, char**)`
     +
     + This function accepts the following non-standard extensions:
     + - `char** envp` as a third parameter
     + - `void` / `noreturn` as return type
     +
     + This function will issue errors for unexpected arguments / return types.
     +/
    extern (D) final void checkMain()
    {
        if (ident != Id.main || isMember() || isNested())
            return; // Not a main function

        TypeFunction tf = type.toTypeFunction();

        Type retType = tf.nextOf();
        if (!retType)
        {
            // auto main(), check after semantic
            assert(this.inferRetType);
            return;
        }

        /// Checks whether `t` is equivalent to `char**`
        /// Ignores qualifiers and treats enums according to their base type
        static bool isCharPtrPtr(Type t)
        {
            auto tp = t.toBasetype().isTypePointer();
            if (!tp)
                return false;

            tp = tp.next.toBasetype().isTypePointer();
            if (!tp)
                return false;

            return tp.next.toBasetype().ty == Tchar;
        }

        // Neither of these qualifiers is allowed because they affect the ABI
        enum invalidSTC = STC.out_ | STC.ref_ | STC.lazy_;

        const nparams = tf.parameterList.length;
        bool argerr;

        const linkage = resolvedLinkage();
        if (linkage == LINK.d)
        {
            if (nparams == 1)
            {
                auto fparam0 = tf.parameterList[0];
                auto t = fparam0.type.toBasetype();
                if (t.ty != Tarray ||
                    t.nextOf().ty != Tarray ||
                    t.nextOf().nextOf().ty != Tchar ||
                    fparam0.storageClass & invalidSTC)
                {
                    argerr = true;
                }
            }

            if (tf.parameterList.varargs || nparams >= 2 || argerr)
                .error(loc, "%s `%s` parameter list must be empty or accept one parameter of type `string[]`", kind, toPrettyChars);
        }

        else if (linkage == LINK.c)
        {
            if (nparams == 2 || nparams == 3)
            {
                // Argument count must be int
                auto argCount = tf.parameterList[0];
                argerr |= !!(argCount.storageClass & invalidSTC);
                argerr |= argCount.type.toBasetype().ty != Tint32;

                // Argument pointer must be char**
                auto argPtr = tf.parameterList[1];
                argerr |= !!(argPtr.storageClass & invalidSTC);
                argerr |= !isCharPtrPtr(argPtr.type);

                // `char** environ` is a common extension, see J.5.1 of the C standard
                if (nparams == 3)
                {
                    auto envPtr = tf.parameterList[2];
                    argerr |= !!(envPtr.storageClass & invalidSTC);
                    argerr |= !isCharPtrPtr(envPtr.type);
                }
            }
            else
                argerr = nparams != 0;

            // Disallow variadic main() - except for K&R declarations in C files.
            // E.g. int main(), int main(argc, argv) int argc, char** argc { ... }
            if (tf.parameterList.varargs && (!this.isCsymbol() || (!tf.parameterList.hasIdentifierList && nparams)))
                argerr |= true;

            if (argerr)
            {
                .error(loc, "%s `%s` parameters must match one of the following signatures", kind, toPrettyChars);
                loc.errorSupplemental("`main()`");
                loc.errorSupplemental("`main(int argc, char** argv)`");
                loc.errorSupplemental("`main(int argc, char** argv, char** environ)` [POSIX extension]");
            }
        }
        else
            return; // Neither C nor D main, ignore (should probably be an error)

        // Allow enums with appropriate base types (same ABI)
        retType = retType.toBasetype();

        if (retType.ty != Tint32 && retType.ty != Tvoid && retType.ty != Tnoreturn)
            .error(loc, "%s `%s` must return `int`, `void` or `noreturn`, not `%s`", kind, toPrettyChars, tf.nextOf().toChars());
    }

    /***********************************************
     * Check all return statements for a function to verify that returning
     * using NRVO is possible.
     *
     * Returns:
     *      `false` if the result cannot be returned by hidden reference.
     */
    extern (D) final bool checkNRVO()
    {
        if (!isNRVO() || returns is null)
            return false;

        auto tf = type.toTypeFunction();
        if (tf.isref)
            return false;

        foreach (rs; *returns)
        {
            if (auto ve = rs.exp.isVarExp())
            {
                auto v = ve.var.isVarDeclaration();
                if (!v || v.isReference())
                    return false;
                else if (nrvo_var is null)
                {
                    // Variables in the data segment (e.g. globals, TLS or not),
                    // parameters and closure variables cannot be NRVOed.
                    if (v.isDataseg() || v.isParameter() || v.toParent2() != this)
                        return false;
                    if (v.nestedrefs.length && needsClosure())
                        return false;
                    // don't know if the return storage is aligned
                    version (MARS)
                    {
                        if (alignSectionVars && (*alignSectionVars).contains(v))
                            return false;
                    }
                    // The variable type needs to be equivalent to the return type.
                    if (!v.type.equivalent(tf.next))
                        return false;
                    //printf("Setting nrvo to %s\n", v.toChars());
                    nrvo_var = v;
                }
                else if (nrvo_var != v)
                    return false;
            }
            else //if (!exp.isLvalue())    // keep NRVO-ability
                return false;
        }
        return true;
    }

    override final inout(FuncDeclaration) isFuncDeclaration() inout
    {
        return this;
    }

    inout(FuncDeclaration) toAliasFunc() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***************************************************
 * Visit each overloaded function/template in turn, and call dg(s) on it.
 * Exit when no more, or dg(s) returns nonzero.
 *
 * Params:
 *  fstart = symbol to start from
 *  dg = the delegate to be called on the overload
 *  sc = context used to check if symbol is accessible (and therefore visible),
 *       can be null
 *
 * Returns:
 *      ==0     continue
 *      !=0     done (and the return value from the last dg() call)
 */
extern (D) int overloadApply(Dsymbol fstart, scope int delegate(Dsymbol) dg, Scope* sc = null)
{
    Dsymbols visited;

    int overloadApplyRecurse(Dsymbol fstart, scope int delegate(Dsymbol) dg, Scope* sc)
    {
        // Detect cyclic calls.
        if (visited.contains(fstart))
            return 0;
        visited.push(fstart);

        Dsymbol next;
        for (auto d = fstart; d; d = next)
        {
            import dmd.access : checkSymbolAccess;
            if (auto od = d.isOverDeclaration())
            {
                /* The scope is needed here to check whether a function in
                   an overload set was added by means of a private alias (or a
                   selective import). If the scope where the alias is created
                   is imported somewhere, the overload set is visible, but the private
                   alias is not.
                */
                if (sc)
                {
                    if (checkSymbolAccess(sc, od))
                    {
                        if (int r = overloadApplyRecurse(od.aliassym, dg, sc))
                            return r;
                    }
                }
                else if (int r = overloadApplyRecurse(od.aliassym, dg, sc))
                    return r;
                next = od.overnext;
            }
            else if (auto fa = d.isFuncAliasDeclaration())
            {
                if (fa.hasOverloads)
                {
                    if (int r = overloadApplyRecurse(fa.funcalias, dg, sc))
                        return r;
                }
                else if (auto fd = fa.toAliasFunc())
                {
                    if (int r = dg(fd))
                        return r;
                }
                else
                {
                    .error(d.loc, "%s `%s` is aliased to a function", d.kind, d.toPrettyChars);
                    break;
                }
                next = fa.overnext;
            }
            else if (auto ad = d.isAliasDeclaration())
            {
                if (sc)
                {
                    if (checkSymbolAccess(sc, ad))
                        next = ad.toAlias();
                }
                else
                   next = ad.toAlias();
                if (next == ad)
                    break;
                if (next == fstart)
                    break;
            }
            else if (auto td = d.isTemplateDeclaration())
            {
                if (int r = dg(td))
                    return r;
                next = td.overnext;
            }
            else if (auto fd = d.isFuncDeclaration())
            {
                if (int r = dg(fd))
                    return r;
                next = fd.overnext;
            }
            else if (auto os = d.isOverloadSet())
            {
                foreach (ds; os.a)
                    if (int r = dg(ds))
                        return r;
            }
            else
            {
                .error(d.loc, "%s `%s` is aliased to a function", d.kind, d.toPrettyChars);
                break;
                // BUG: should print error message?
            }
        }
        return 0;
    }
    return overloadApplyRecurse(fstart, dg, sc);
}

/**
Checks for mismatching modifiers between `lhsMod` and `rhsMod` and prints the
mismatching modifiers to `buf`.

The modifiers of the `lhsMod` mismatching the ones with the `rhsMod` are printed, i.e.
lhs(shared) vs. rhs() prints "`shared`", wheras lhs() vs rhs(shared) prints "non-shared".

Params:
    buf = output buffer to write to
    lhsMod = modifier on the left-hand side
    lhsMod = modifier on the right-hand side

Returns:

A tuple with `isMutable` and `isNotShared` set
if the `lhsMod` is missing those modifiers (compared to rhs).
*/
auto MODMatchToBuffer(OutBuffer* buf, ubyte lhsMod, ubyte rhsMod)
{
    static struct Mismatches
    {
        bool isNotShared;
        bool isMutable;
    }

    Mismatches mismatches;

    bool bothMutable = ((lhsMod & rhsMod) == 0);
    bool sharedMismatch = ((lhsMod ^ rhsMod) & MODFlags.shared_) != 0;
    bool sharedMismatchOnly = ((lhsMod ^ rhsMod) == MODFlags.shared_);

    if (lhsMod & MODFlags.shared_)
        buf.writestring("`shared` ");
    else if (sharedMismatch && !(lhsMod & MODFlags.immutable_))
    {
        buf.writestring("non-shared ");
        mismatches.isNotShared = true;
    }

    if (bothMutable && sharedMismatchOnly)
    {
    }
    else if (lhsMod & MODFlags.immutable_)
        buf.writestring("`immutable` ");
    else if (lhsMod & MODFlags.const_)
        buf.writestring("`const` ");
    else if (lhsMod & MODFlags.wild)
        buf.writestring("`inout` ");
    else
    {
        buf.writestring("mutable ");
        mismatches.isMutable = true;
    }

    return mismatches;
}

///
unittest
{
    OutBuffer buf;
    auto mismatches = MODMatchToBuffer(&buf, MODFlags.shared_, 0);
    assert(buf[] == "`shared` ");
    assert(!mismatches.isNotShared);

    buf.setsize(0);
    mismatches = MODMatchToBuffer(&buf, 0, MODFlags.shared_);
    assert(buf[] == "non-shared ");
    assert(mismatches.isNotShared);

    buf.setsize(0);
    mismatches = MODMatchToBuffer(&buf, MODFlags.const_, 0);
    assert(buf[] == "`const` ");
    assert(!mismatches.isMutable);

    buf.setsize(0);
    mismatches = MODMatchToBuffer(&buf, 0, MODFlags.const_);
    assert(buf[] == "mutable ");
    assert(mismatches.isMutable);
}

/**************************************
 * Returns an indirect type one step from t.
 */
Type getIndirection(Type t)
{
    t = t.baseElemOf();
    if (t.ty == Tarray || t.ty == Tpointer)
        return t.nextOf().toBasetype();
    if (t.ty == Taarray || t.ty == Tclass)
        return t;
    if (t.ty == Tstruct)
        return t.hasPointers() ? t : null; // TODO

    // should consider TypeDelegate?
    return null;
}

/**************************************
 * Performs type-based alias analysis between a newly created value and a pre-
 * existing memory reference:
 *
 * Assuming that a reference A to a value of type `ta` was available to the code
 * that created a reference B to a value of type `tb`, it returns whether B
 * might alias memory reachable from A based on the types involved (either
 * directly or via any number of indirections in either A or B).
 *
 * This relation is not symmetric in the two arguments. For example, a
 * a `const(int)` reference can point to a pre-existing `int`, but not the other
 * way round.
 *
 * Examples:
 *
 *      ta,           tb,               result
 *      `const(int)`, `int`,            `false`
 *      `int`,        `const(int)`,     `true`
 *      `int`,        `immutable(int)`, `false`
 *      const(immutable(int)*), immutable(int)*, false   // BUG: returns true
 *
 * Params:
 *      ta = value type being referred to
 *      tb = referred to value type that could be constructed from ta
 *
 * Returns:
 *      true if reference to `tb` is isolated from reference to `ta`
 */
private bool traverseIndirections(Type ta, Type tb)
{
    //printf("traverseIndirections(%s, %s)\n", ta.toChars(), tb.toChars());

    static bool traverse(Type ta, Type tb, ref scope AssocArray!(const(char)*, bool) table, bool reversePass)
    {
        //printf("traverse(%s, %s)\n", ta.toChars(), tb.toChars());
        ta = ta.baseElemOf();
        tb = tb.baseElemOf();

        // First, check if the pointed-to types are convertible to each other such
        // that they might alias directly.
        static bool mayAliasDirect(Type source, Type target)
        {
            return
                // if source is the same as target or can be const-converted to target
                source.constConv(target) != MATCH.nomatch ||
                // if target is void and source can be const-converted to target
                (target.ty == Tvoid && MODimplicitConv(source.mod, target.mod));
        }

        if (mayAliasDirect(reversePass ? tb : ta, reversePass ? ta : tb))
        {
            //printf(" true  mayalias %s %s %d\n", ta.toChars(), tb.toChars(), reversePass);
            return false;
        }
        if (ta.nextOf() && ta.nextOf() == tb.nextOf())
        {
             //printf(" next==next %s %s %d\n", ta.toChars(), tb.toChars(), reversePass);
             return true;
        }

        if (tb.ty == Tclass || tb.ty == Tstruct)
        {
            /* Traverse the type of each field of the aggregate
             */
            bool* found = table.getLvalue(tb.deco);
            if (*found == true)
                return true; // We have already seen this symbol, break the cycle
            else
                *found = true;

            AggregateDeclaration sym = tb.toDsymbol(null).isAggregateDeclaration();
            foreach (v; sym.fields)
            {
                Type tprmi = v.type.addMod(tb.mod);
                //printf("\ttb = %s, tprmi = %s\n", tb.toChars(), tprmi.toChars());
                if (!traverse(ta, tprmi, table, reversePass))
                    return false;
            }
        }
        else if (tb.ty == Tarray || tb.ty == Taarray || tb.ty == Tpointer)
        {
            Type tind = tb.nextOf();
            if (!traverse(ta, tind, table, reversePass))
                return false;
        }
        else if (tb.hasPointers())
        {
            // BUG: consider the context pointer of delegate types
            return false;
        }

        // Still no match, so try breaking up ta if we have not done so yet.
        if (!reversePass)
        {
            scope newTable = AssocArray!(const(char)*, bool)();
            return traverse(tb, ta, newTable, true);
        }

        return true;
    }

    // To handle arbitrary levels of indirections in both parameters, we
    // recursively descend into aggregate members/levels of indirection in both
    // `ta` and `tb` while avoiding cycles. Start with the original types.
    scope table = AssocArray!(const(char)*, bool)();
    const result = traverse(ta, tb, table, false);
    //printf("  returns %d\n", result);
    return result;
}

/* For all functions between outerFunc and f, mark them as needing
 * a closure.
 */
private void markAsNeedingClosure(Dsymbol f, FuncDeclaration outerFunc)
{
    for (Dsymbol sx = f; sx && sx != outerFunc; sx = sx.toParentP(outerFunc))
    {
        FuncDeclaration fy = sx.isFuncDeclaration();
        if (fy && fy.closureVars.length)
        {
            /* fy needs a closure if it has closureVars[],
             * because the frame pointer in the closure will be accessed.
             */
            fy.requiresClosure = true;
        }
    }
}

/********
 * Given a nested function f inside a function outerFunc, check
 * if any sibling callers of f have escaped. If so, mark
 * all the enclosing functions as needing closures.
 * This is recursive: we need to check the callers of our siblings.
 * Note that nested functions can only call lexically earlier nested
 * functions, so loops are impossible.
 * Params:
 *      f = inner function (nested within outerFunc)
 *      outerFunc = outer function
 *      p = for internal recursion use
 * Returns:
 *      true if any closures were needed
 */
private bool checkEscapingSiblings(FuncDeclaration f, FuncDeclaration outerFunc, void* p = null)
{
    static struct PrevSibling
    {
        PrevSibling* p;
        FuncDeclaration f;
    }

    if (f.computedEscapingSiblings)
        return f.hasEscapingSiblings;

    PrevSibling ps;
    ps.p = cast(PrevSibling*)p;
    ps.f = f;

    //printf("checkEscapingSiblings(f = %s, outerfunc = %s)\n", f.toChars(), outerFunc.toChars());
    bool bAnyClosures = false;
    for (size_t i = 0; i < f.siblingCallers.length; ++i)
    {
        FuncDeclaration g = f.siblingCallers[i];
        if (g.isThis() || g.tookAddressOf)
        {
            markAsNeedingClosure(g, outerFunc);
            bAnyClosures = true;
        }

        for (auto parent = g.toParentP(outerFunc); parent && parent !is outerFunc; parent = parent.toParentP(outerFunc))
        {
            // A parent of the sibling had its address taken.
            // Assume escaping of parent affects its children, so needs propagating.
            // see https://issues.dlang.org/show_bug.cgi?id=19679
            FuncDeclaration parentFunc = parent.isFuncDeclaration;
            if (parentFunc && parentFunc.tookAddressOf)
            {
                markAsNeedingClosure(parentFunc, outerFunc);
                bAnyClosures = true;
            }
        }

        PrevSibling* prev = cast(PrevSibling*)p;
        while (1)
        {
            if (!prev)
            {
                bAnyClosures |= checkEscapingSiblings(g, outerFunc, &ps);
                break;
            }
            if (prev.f == g)
                break;
            prev = prev.p;
        }
    }
    f.hasEscapingSiblings = bAnyClosures;
    f.computedEscapingSiblings = true;
    //printf("\t%d\n", bAnyClosures);
    return bAnyClosures;
}

/***********************************************************
 * Used as a way to import a set of functions from another scope into this one.
 */
extern (C++) final class FuncAliasDeclaration : FuncDeclaration
{
    FuncDeclaration funcalias;
    bool hasOverloads;

    extern (D) this(Identifier ident, FuncDeclaration funcalias, bool hasOverloads = true)
    {
        super(funcalias.loc, funcalias.endloc, ident, funcalias.storage_class, funcalias.type);
        assert(funcalias != this);
        this.funcalias = funcalias;

        this.hasOverloads = hasOverloads;
        if (hasOverloads)
        {
            if (FuncAliasDeclaration fad = funcalias.isFuncAliasDeclaration())
                this.hasOverloads = fad.hasOverloads;
        }
        else
        {
            // for internal use
            assert(!funcalias.isFuncAliasDeclaration());
            this.hasOverloads = false;
        }
        userAttribDecl = funcalias.userAttribDecl;
    }

    override inout(FuncAliasDeclaration) isFuncAliasDeclaration() inout
    {
        return this;
    }

    override const(char)* kind() const
    {
        return "function alias";
    }

    override inout(FuncDeclaration) toAliasFunc() inout
    {
        return funcalias.toAliasFunc();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class FuncLiteralDeclaration : FuncDeclaration
{
    TOK tok;        // TOK.function_ or TOK.delegate_
    Type treq;      // target of return type inference

    // backend
    bool deferToObj;

    extern (D) this(const ref Loc loc, const ref Loc endloc, Type type, TOK tok, ForeachStatement fes, Identifier id = null, StorageClass storage_class = STC.undefined_)
    {
        super(loc, endloc, null, storage_class, type);
        this.ident = id ? id : Id.empty;
        this.tok = tok;
        this.fes = fes;
        // Always infer scope for function literals
        // See https://issues.dlang.org/show_bug.cgi?id=20362
        this.inferScope = true;
        //printf("FuncLiteralDeclaration() id = '%s', type = '%s'\n", this.ident.toChars(), type.toChars());
    }

    override FuncLiteralDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("FuncLiteralDeclaration::syntaxCopy('%s')\n", toChars());
        assert(!s);
        auto f = new FuncLiteralDeclaration(loc, endloc, type.syntaxCopy(), tok, fes, ident, storage_class & STC.auto_);
        f.treq = treq; // don't need to copy
        FuncDeclaration.syntaxCopy(f);
        return f;
    }

    override bool isNested() const
    {
        //printf("FuncLiteralDeclaration::isNested() '%s'\n", toChars());
        return (tok != TOK.function_) && !isThis();
    }

    override inout(AggregateDeclaration) isThis() inout
    {
        return tok == TOK.delegate_ ? super.isThis() : null;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    /*******************************
     * Modify all expression type of return statements to tret.
     *
     * On function literals, return type may be modified based on the context type
     * after its semantic3 is done, in FuncExp::implicitCastTo.
     *
     *  A function() dg = (){ return new B(); } // OK if is(B : A) == true
     *
     * If B to A conversion is convariant that requires offseet adjusting,
     * all return statements should be adjusted to return expressions typed A.
     */
    extern (D) void modifyReturns(Scope* sc, Type tret)
    {
        import dmd.statement_rewrite_walker;

        extern (C++) final class RetWalker : StatementRewriteWalker
        {
            alias visit = typeof(super).visit;
        public:
            Scope* sc;
            Type tret;
            FuncLiteralDeclaration fld;

            override void visit(ReturnStatement s)
            {
                Expression exp = s.exp;
                if (exp && !exp.type.equals(tret))
                    s.exp = exp.implicitCastTo(sc, tret);
            }
        }

        if (semanticRun < PASS.semantic3done)
            return;

        if (fes)
            return;

        scope RetWalker w = new RetWalker();
        w.sc = sc;
        w.tret = tret;
        w.fld = this;
        fbody.accept(w);

        // Also update the inferred function type to match the new return type.
        // This is required so the code generator does not try to cast the
        // modified returns back to the original type.
        if (inferRetType && type.nextOf() != tret)
            type.toTypeFunction().next = tret;
    }

    override inout(FuncLiteralDeclaration) isFuncLiteralDeclaration() inout
    {
        return this;
    }

    override const(char)* kind() const
    {
        // GCC requires the (char*) casts
        return (tok != TOK.function_) ? "delegate" : "function";
    }

    override const(char)* toPrettyChars(bool QualifyTypes = false)
    {
        if (parent)
        {
            TemplateInstance ti = parent.isTemplateInstance();
            if (ti)
                return ti.tempdecl.toPrettyChars(QualifyTypes);
        }
        return Dsymbol.toPrettyChars(QualifyTypes);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class CtorDeclaration : FuncDeclaration
{
    bool isCpCtor;
    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc, Type type, bool isCpCtor = false)
    {
        super(loc, endloc, Id.ctor, stc, type);
        this.isCpCtor = isCpCtor;
        //printf("CtorDeclaration(loc = %s) %s %p\n", loc.toChars(), toChars(), this);
    }

    override CtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto f = new CtorDeclaration(loc, endloc, storage_class, type.syntaxCopy());
        FuncDeclaration.syntaxCopy(f);
        return f;
    }

    override const(char)* kind() const
    {
        return isCpCtor ? "copy constructor" : "constructor";
    }

    override const(char)* toChars() const
    {
        return "this";
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return (isThis() && vthis && global.params.useInvariants == CHECKENABLE.on);
    }

    override inout(CtorDeclaration) isCtorDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class PostBlitDeclaration : FuncDeclaration
{
    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc, Identifier id)
    {
        super(loc, endloc, id, stc, null);
    }

    override PostBlitDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto dd = new PostBlitDeclaration(loc, endloc, storage_class, ident);
        FuncDeclaration.syntaxCopy(dd);
        return dd;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return (isThis() && vthis && global.params.useInvariants == CHECKENABLE.on);
    }

    override bool overloadInsert(Dsymbol s)
    {
        return false; // cannot overload postblits
    }

    override inout(PostBlitDeclaration) isPostBlitDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DtorDeclaration : FuncDeclaration
{
    extern (D) this(const ref Loc loc, const ref Loc endloc)
    {
        super(loc, endloc, Id.dtor, STC.undefined_, null);
    }

    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc, Identifier id)
    {
        super(loc, endloc, id, stc, null);
    }

    override DtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto dd = new DtorDeclaration(loc, endloc, storage_class, ident);
        FuncDeclaration.syntaxCopy(dd);
        return dd;
    }

    override const(char)* kind() const
    {
        return "destructor";
    }

    override const(char)* toChars() const
    {
        return "~this";
    }

    override bool isVirtual() const
    {
        // D dtor's don't get put into the vtbl[]
        // this is a hack so that extern(C++) destructors report as virtual, which are manually added to the vtable
        return vtblIndex != -1;
    }

    override bool addPreInvariant()
    {
        return (isThis() && vthis && global.params.useInvariants == CHECKENABLE.on);
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override bool overloadInsert(Dsymbol s)
    {
        return false; // cannot overload destructors
    }

    override inout(DtorDeclaration) isDtorDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class StaticCtorDeclaration : FuncDeclaration
{
    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc("_staticCtor", loc), STC.static_ | stc, null);
    }

    extern (D) this(const ref Loc loc, const ref Loc endloc, string name, StorageClass stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc(name, loc), STC.static_ | stc, null);
    }

    override StaticCtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto scd = new StaticCtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(scd);
        return scd;
    }

    override final inout(AggregateDeclaration) isThis() inout @nogc nothrow pure @safe
    {
        return null;
    }

    override final bool isVirtual() const @nogc nothrow pure @safe
    {
        return false;
    }

    override final bool addPreInvariant() @nogc nothrow pure @safe
    {
        return false;
    }

    override final bool addPostInvariant() @nogc nothrow pure @safe
    {
        return false;
    }

    override final bool hasStaticCtorOrDtor() @nogc nothrow pure @safe
    {
        return true;
    }

    override final inout(StaticCtorDeclaration) isStaticCtorDeclaration() inout @nogc nothrow pure @safe
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class SharedStaticCtorDeclaration : StaticCtorDeclaration
{
    /// Exclude this constructor from cyclic dependency check
    bool standalone;

    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc)
    {
        super(loc, endloc, "_sharedStaticCtor", stc);
    }

    override SharedStaticCtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto scd = new SharedStaticCtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(scd);
        return scd;
    }

    override inout(SharedStaticCtorDeclaration) isSharedStaticCtorDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class StaticDtorDeclaration : FuncDeclaration
{
    VarDeclaration vgate; // 'gate' variable

    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc("_staticDtor", loc), STC.static_ | stc, null);
    }

    extern (D) this(const ref Loc loc, const ref Loc endloc, string name, StorageClass stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc(name, loc), STC.static_ | stc, null);
    }

    override StaticDtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto sdd = new StaticDtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(sdd);
        return sdd;
    }

    override final inout(AggregateDeclaration) isThis() inout
    {
        return null;
    }

    override final bool isVirtual() const
    {
        return false;
    }

    override final bool hasStaticCtorOrDtor()
    {
        return true;
    }

    override final bool addPreInvariant()
    {
        return false;
    }

    override final bool addPostInvariant()
    {
        return false;
    }

    override final inout(StaticDtorDeclaration) isStaticDtorDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class SharedStaticDtorDeclaration : StaticDtorDeclaration
{
    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc)
    {
        super(loc, endloc, "_sharedStaticDtor", stc);
    }

    override SharedStaticDtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto sdd = new SharedStaticDtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(sdd);
        return sdd;
    }

    override inout(SharedStaticDtorDeclaration) isSharedStaticDtorDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class InvariantDeclaration : FuncDeclaration
{
    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc, Identifier id, Statement fbody)
    {
        // Make a unique invariant for now; we'll fix it up as we add it to the aggregate invariant list.
        super(loc, endloc, id ? id : Identifier.generateId("__invariant"), stc, null);
        this.fbody = fbody;
    }

    override InvariantDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto id = new InvariantDeclaration(loc, endloc, storage_class, null, null);
        FuncDeclaration.syntaxCopy(id);
        return id;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override inout(InvariantDeclaration) isInvariantDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    extern (D) void fixupInvariantIdent(size_t offset)
    {
        OutBuffer idBuf;
        idBuf.writestring("__invariant");
        idBuf.print(offset);

        ident = Identifier.idPool(idBuf[]);
    }
}


/***********************************************************
 */
extern (C++) final class UnitTestDeclaration : FuncDeclaration
{
    char* codedoc;      // for documented unittest

    // toObjFile() these nested functions after this one
    FuncDeclarations deferredNested;

    extern (D) this(const ref Loc loc, const ref Loc endloc, StorageClass stc, char* codedoc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc("__unittest", loc), stc, null);
        this.codedoc = codedoc;
    }

    override UnitTestDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto utd = new UnitTestDeclaration(loc, endloc, storage_class, codedoc);
        FuncDeclaration.syntaxCopy(utd);
        return utd;
    }

    override inout(AggregateDeclaration) isThis() inout
    {
        return null;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override inout(UnitTestDeclaration) isUnitTestDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class NewDeclaration : FuncDeclaration
{
    extern (D) this(const ref Loc loc, StorageClass stc)
    {
        super(loc, Loc.initial, Id.classNew, STC.static_ | stc, null);
    }

    override NewDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto f = new NewDeclaration(loc, storage_class);
        FuncDeclaration.syntaxCopy(f);
        return f;
    }

    override const(char)* kind() const
    {
        return "allocator";
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override inout(NewDeclaration) isNewDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**************************************
 * When a traits(compiles) is used on a function literal call
 * we need to take into account if the body of the function
 * violates any attributes, however, we must not affect the
 * attribute inference on the outer function. The attributes
 * of the function literal still need to be inferred, therefore
 * we need a way to check for the scope that the traits compiles
 * introduces.
 *
 * Params:
 *   sc = scope to be checked for
 *
 * Returns: `true` if the provided scope is the root
 * of the traits compiles list of scopes.
 */
bool isRootTraitsCompilesScope(Scope* sc)
{
    return (sc.flags & SCOPE.compile) && !(sc.func.flags & SCOPE.compile);
}

/**************************************
 * A statement / expression in this scope is not `@safe`,
 * so mark the enclosing function as `@system`
 *
 * Params:
 *   sc = scope that the unsafe statement / expression is in
 *   gag = surpress error message (used in escape.d)
 *   loc = location of error
 *   fmt = printf-style format string
 *   arg0  = (optional) argument for first %s format specifier
 *   arg1  = (optional) argument for second %s format specifier
 *   arg2  = (optional) argument for third %s format specifier
 * Returns: whether there's a safe error
 */
bool setUnsafe(Scope* sc,
    bool gag = false, Loc loc = Loc.init, const(char)* fmt = null,
    RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
{
    if (sc.intypeof)
        return false; // typeof(cast(int*)0) is safe

    if (sc.flags & SCOPE.debug_) // debug {} scopes are permissive
        return false;

    if (!sc.func)
    {
        if (sc.varDecl)
        {
            if (sc.varDecl.storage_class & STC.safe)
            {
                .error(loc, fmt, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
                return true;
            }
            else if (!(sc.varDecl.storage_class & STC.trusted))
            {
                sc.varDecl.storage_class |= STC.system;
                sc.varDecl.systemInferred = true;
            }
        }
        return false;
    }


    if (isRootTraitsCompilesScope(sc)) // __traits(compiles, x)
    {
        if (sc.func.isSafeBypassingInference())
        {
            // Message wil be gagged, but still call error() to update global.errors and for
            // -verrors=spec
            .error(loc, fmt, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
            return true;
        }
        return false;
    }

    return sc.func.setUnsafe(gag, loc, fmt, arg0, arg1, arg2);
}

/***************************************
 * Like `setUnsafe`, but for safety errors still behind preview switches
 *
 * Given a `FeatureState fs`, for example dip1000 / dip25 / systemVariables,
 * the behavior changes based on the setting:
 *
 * - In case of `-revert=fs`, it does nothing.
 * - In case of `-preview=fs`, it's the same as `setUnsafe`
 * - By default, print a deprecation in `@safe` functions, or store an attribute violation in inferred functions.
 *
 * Params:
 *   sc = used to find affected function/variable, and for checking whether we are in a deprecated / speculative scope
 *   fs = feature state from the preview flag
 *   gag = surpress error message
 *   loc = location of error
 *   msg = printf-style format string
 *   arg0  = (optional) argument for first %s format specifier
 *   arg1  = (optional) argument for second %s format specifier
 *   arg2  = (optional) argument for third %s format specifier
 * Returns: whether an actual safe error (not deprecation) occured
 */
bool setUnsafePreview(Scope* sc, FeatureState fs, bool gag, Loc loc, const(char)* msg,
    RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
{
    //printf("setUnsafePreview() fs:%d %s\n", fs, msg);
    with (FeatureState) final switch (fs)
    {
      case disabled:
        return false;

      case enabled:
        return sc.setUnsafe(gag, loc, msg, arg0, arg1, arg2);

      case default_:
        if (!sc.func)
            return false;
        if (sc.func.isSafeBypassingInference())
        {
            if (!gag && !sc.isDeprecated())
            {
                deprecation(loc, msg, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
            }
        }
        else if (!sc.func.safetyViolation)
        {
            import dmd.func : AttributeViolation;
            sc.func.safetyViolation = new AttributeViolation(loc, msg, arg0, arg1, arg2);
        }
        return false;
    }
}

/// Stores a reason why a function failed to infer a function attribute like `@safe` or `pure`
///
/// Has two modes:
/// - a regular safety error, stored in (fmtStr, arg0, arg1)
/// - a call to a function without the attribute, which is a special case, because in that case,
///   that function might recursively also have a `AttributeViolation`. This way, in case
///   of a big call stack, the error can go down all the way to the root cause.
///   The `FunctionDeclaration` is then stored in `arg0` and `fmtStr` must be `null`.
struct AttributeViolation
{
    /// location of error
    Loc loc = Loc.init;
    /// printf-style format string
    const(char)* fmtStr = null;
    /// Arguments for up to two `%s` format specifiers in format string
    RootObject arg0 = null;
    /// ditto
    RootObject arg1 = null;
    /// ditto
    RootObject arg2 = null;
}

/// Print the reason why `fd` was inferred `@system` as a supplemental error
/// Params:
///   fd = function to check
///   maxDepth = up to how many functions deep to report errors
///   deprecation = print deprecations instead of errors
///   stc = storage class of attribute to check
void errorSupplementalInferredAttr(FuncDeclaration fd, int maxDepth, bool deprecation, STC stc)
{
    auto errorFunc = deprecation ? &deprecationSupplemental : &errorSupplemental;

    AttributeViolation* s;
    const(char)* attr;
    if (stc & STC.safe)
    {
        s = fd.safetyViolation;
        attr = "@safe";
    }
    else if (stc & STC.pure_)
    {
        s = fd.pureViolation;
        attr = "pure";
    }
    else if (stc & STC.nothrow_)
    {
        s = fd.nothrowViolation;
        attr = "nothrow";
    }
    else if (stc & STC.nogc)
    {
        s = fd.nogcViolation;
        attr = "@nogc";
    }

    if (s)
    {
        if (s.fmtStr)
        {
            errorFunc(s.loc, deprecation ?
                "which wouldn't be `%s` because of:" :
                "which wasn't inferred `%s` because of:", attr);
            if (stc == STC.nogc || stc == STC.pure_)
            {
                auto f = (cast(Dsymbol) s.arg0).isFuncDeclaration();
                errorFunc(s.loc, s.fmtStr, f.kind(), f.toPrettyChars(), s.arg1 ? s.arg1.toChars() : "");
            }
            else
            {
                errorFunc(s.loc, s.fmtStr,
                    s.arg0 ? s.arg0.toChars() : "", s.arg1 ? s.arg1.toChars() : "", s.arg2 ? s.arg2.toChars() : "");
            }
        }
        else if (auto sa = s.arg0.isDsymbol())
        {
            if (FuncDeclaration fd2 = sa.isFuncDeclaration())
            {
                if (maxDepth > 0)
                {
                    errorFunc(s.loc, "which calls `%s`", fd2.toPrettyChars());
                    errorSupplementalInferredAttr(fd2, maxDepth - 1, deprecation, stc);
                }
            }
        }
    }
}
