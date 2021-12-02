/**
 * A scope as defined by curly braces `{}`.
 *
 * Not to be confused with the `scope` storage class.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dscope.d, _dscope.d)
 * Documentation:  https://dlang.org/phobos/dmd_dscope.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dscope.d
 */

module dmd.dscope;

import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.ctorflow;
import dmd.dclass;
import dmd.declaration;
import dmd.dmodule;
import dmd.doc;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.expression;
import dmd.errors;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.root.outbuffer;
import dmd.root.rmem;
import dmd.root.speller;
import dmd.statement;
import dmd.target;
import dmd.tokens;

//version=LOGSEARCH;


// List of flags that can be applied to this `Scope`
enum SCOPE
{
    ctor          = 0x0001,   /// constructor type
    noaccesscheck = 0x0002,   /// don't do access checks
    condition     = 0x0004,   /// inside static if/assert condition
    debug_        = 0x0008,   /// inside debug conditional
    constraint    = 0x0010,   /// inside template constraint
    invariant_    = 0x0020,   /// inside invariant code
    require       = 0x0040,   /// inside in contract code
    ensure        = 0x0060,   /// inside out contract code
    contract      = 0x0060,   /// [mask] we're inside contract code
    ctfe          = 0x0080,   /// inside a ctfe-only expression
    compile       = 0x0100,   /// inside __traits(compile)
    ignoresymbolvisibility    = 0x0200,   /// ignore symbol visibility
                                          /// https://issues.dlang.org/show_bug.cgi?id=15907
    onlysafeaccess = 0x0400,  /// unsafe access is not allowed for @safe code
    Cfile         = 0x0800,   /// C semantics apply
    free          = 0x8000,   /// is on free list

    fullinst      = 0x10000,  /// fully instantiate templates
    alias_        = 0x20000,  /// inside alias declaration.

    // The following are mutually exclusive
    printf        = 0x4_0000, /// printf-style function
    scanf         = 0x8_0000, /// scanf-style function
}

/// Flags that are carried along with a scope push()
private enum PersistentFlags =
    SCOPE.contract | SCOPE.debug_ | SCOPE.ctfe | SCOPE.compile | SCOPE.constraint |
    SCOPE.noaccesscheck | SCOPE.onlysafeaccess | SCOPE.ignoresymbolvisibility |
    SCOPE.printf | SCOPE.scanf | SCOPE.Cfile;

struct Scope
{
    Scope* enclosing;               /// enclosing Scope

    Module _module;                 /// Root module
    ScopeDsymbol scopesym;          /// current symbol
    FuncDeclaration func;           /// function we are in
    Dsymbol parent;                 /// parent to use
    LabelStatement slabel;          /// enclosing labelled statement
    SwitchStatement sw;             /// enclosing switch statement
    Statement tryBody;              /// enclosing _body of TryCatchStatement or TryFinallyStatement
    TryFinallyStatement tf;         /// enclosing try finally statement
    ScopeGuardStatement os;            /// enclosing scope(xxx) statement
    Statement sbreak;               /// enclosing statement that supports "break"
    Statement scontinue;            /// enclosing statement that supports "continue"
    ForeachStatement fes;           /// if nested function for ForeachStatement, this is it
    Scope* callsc;                  /// used for __FUNCTION__, __PRETTY_FUNCTION__ and __MODULE__
    Dsymbol inunion;                /// != null if processing members of a union
    bool nofree;                    /// true if shouldn't free it
    bool inLoop;                    /// true if inside a loop (where constructor calls aren't allowed)
    int intypeof;                   /// in typeof(exp)
    VarDeclaration lastVar;         /// Previous symbol used to prevent goto-skips-init

    /* If  minst && !tinst, it's in definitely non-speculative scope (eg. module member scope).
     * If !minst && !tinst, it's in definitely speculative scope (eg. template constraint).
     * If  minst &&  tinst, it's in instantiated code scope without speculation.
     * If !minst &&  tinst, it's in instantiated code scope with speculation.
     */
    Module minst;                   /// root module where the instantiated templates should belong to
    TemplateInstance tinst;         /// enclosing template instance

    CtorFlow ctorflow;              /// flow analysis for constructors

    /// alignment for struct members
    AlignDeclaration aligndecl;

    /// C++ namespace this symbol is in
    CPPNamespaceDeclaration namespace;

    /// linkage for external functions
    LINK linkage = LINK.d;

    /// mangle type
    CPPMANGLE cppmangle = CPPMANGLE.def;

    /// inlining strategy for functions
    PragmaDeclaration inlining;

    /// visibility for class members
    Visibility visibility = Visibility(Visibility.Kind.public_);
    int explicitVisibility;         /// set if in an explicit visibility attribute

    StorageClass stc;               /// storage class

    DeprecatedDeclaration depdecl;  /// customized deprecation message

    uint flags;

    // user defined attributes
    UserAttributeDeclaration userAttribDecl;

    DocComment* lastdc;        /// documentation comment for last symbol at this scope
    uint[void*] anchorCounts;  /// lookup duplicate anchor name count
    Identifier prevAnchor;     /// qualified symbol name of last doc anchor

    AliasDeclaration aliasAsg; /// if set, then aliasAsg is being assigned a new value,
                               /// do not set wasRead for it

    extern (D) __gshared Scope* freelist;

    extern (D) static Scope* alloc()
    {
        if (freelist)
        {
            Scope* s = freelist;
            freelist = s.enclosing;
            //printf("freelist %p\n", s);
            assert(s.flags & SCOPE.free);
            s.flags &= ~SCOPE.free;
            return s;
        }
        return new Scope();
    }

    extern (D) static Scope* createGlobal(Module _module)
    {
        Scope* sc = Scope.alloc();
        *sc = Scope.init;
        sc._module = _module;
        sc.minst = _module;
        sc.scopesym = new ScopeDsymbol();
        sc.scopesym.symtab = new DsymbolTable();
        // Add top level package as member of this global scope
        Dsymbol m = _module;
        while (m.parent)
            m = m.parent;
        m.addMember(null, sc.scopesym);
        m.parent = null; // got changed by addMember()
        if (_module.isCFile)
            sc.flags |= SCOPE.Cfile;
        // Create the module scope underneath the global scope
        sc = sc.push(_module);
        sc.parent = _module;
        return sc;
    }

    extern (C++) Scope* copy()
    {
        Scope* sc = Scope.alloc();
        *sc = this;
        /* https://issues.dlang.org/show_bug.cgi?id=11777
         * The copied scope should not inherit fieldinit.
         */
        sc.ctorflow.fieldinit = null;
        return sc;
    }

    extern (C++) Scope* push()
    {
        Scope* s = copy();
        //printf("Scope::push(this = %p) new = %p\n", this, s);
        assert(!(flags & SCOPE.free));
        s.scopesym = null;
        s.enclosing = &this;
        debug
        {
            if (enclosing)
                assert(!(enclosing.flags & SCOPE.free));
            if (s == enclosing)
            {
                printf("this = %p, enclosing = %p, enclosing.enclosing = %p\n", s, &this, enclosing);
            }
            assert(s != enclosing);
        }
        s.slabel = null;
        s.nofree = false;
        s.ctorflow.fieldinit = ctorflow.fieldinit.arraydup;
        s.flags = (flags & PersistentFlags);
        s.lastdc = null;
        assert(&this != s);
        return s;
    }

    extern (C++) Scope* push(ScopeDsymbol ss)
    {
        //printf("Scope::push(%s)\n", ss.toChars());
        Scope* s = push();
        s.scopesym = ss;
        return s;
    }

    extern (C++) Scope* pop()
    {
        //printf("Scope::pop() %p nofree = %d\n", this, nofree);
        if (enclosing)
            enclosing.ctorflow.OR(ctorflow);
        ctorflow.freeFieldinit();

        Scope* enc = enclosing;
        if (!nofree)
        {
            if (mem.isGCEnabled)
                this = this.init;
            enclosing = freelist;
            freelist = &this;
            flags |= SCOPE.free;
        }
        return enc;
    }

    /*************************
     * Similar to pop(), but the results in `this` are not folded
     * into `enclosing`.
     */
    extern (D) void detach()
    {
        ctorflow.freeFieldinit();
        enclosing = null;
        pop();
    }

    extern (C++) Scope* startCTFE()
    {
        Scope* sc = this.push();
        sc.flags = this.flags | SCOPE.ctfe;
        version (none)
        {
            /* TODO: Currently this is not possible, because we need to
             * unspeculative some types and symbols if they are necessary for the
             * final executable. Consider:
             *
             * struct S(T) {
             *   string toString() const { return "instantiated"; }
             * }
             * enum x = S!int();
             * void main() {
             *   // To call x.toString in runtime, compiler should unspeculative S!int.
             *   assert(x.toString() == "instantiated");
             * }
             */
            // If a template is instantiated from CT evaluated expression,
            // compiler can elide its code generation.
            sc.tinst = null;
            sc.minst = null;
        }
        return sc;
    }

    extern (C++) Scope* endCTFE()
    {
        assert(flags & SCOPE.ctfe);
        return pop();
    }


    /*******************************
     * Merge results of `ctorflow` into `this`.
     * Params:
     *   loc = for error messages
     *   ctorflow = flow results to merge in
     */
    extern (D) void merge(const ref Loc loc, const ref CtorFlow ctorflow)
    {
        if (!mergeCallSuper(this.ctorflow.callSuper, ctorflow.callSuper))
            error(loc, "one path skips constructor");

        const fies = ctorflow.fieldinit;
        if (this.ctorflow.fieldinit.length && fies.length)
        {
            FuncDeclaration f = func;
            if (fes)
                f = fes.func;
            auto ad = f.isMemberDecl();
            assert(ad);
            foreach (i, v; ad.fields)
            {
                bool mustInit = (v.storage_class & STC.nodefaultctor || v.type.needsNested());
                auto fieldInit = &this.ctorflow.fieldinit[i];
                const fiesCurrent = fies[i];
                if (fieldInit.loc is Loc.init)
                    fieldInit.loc = fiesCurrent.loc;
                if (!mergeFieldInit(this.ctorflow.fieldinit[i].csx, fiesCurrent.csx) && mustInit)
                {
                    error(loc, "one path skips field `%s`", v.toChars());
                }
            }
        }
    }

    /************************************
     * Perform unqualified name lookup by following the chain of scopes up
     * until found.
     *
     * Params:
     *  loc = location to use for error messages
     *  ident = name to look up
     *  pscopesym = if supplied and name is found, set to scope that ident was found in
     *  flags = modify search based on flags
     *
     * Returns:
     *  symbol if found, null if not
     */
    extern (C++) Dsymbol search(const ref Loc loc, Identifier ident, Dsymbol* pscopesym, int flags = IgnoreNone)
    {
        version (LOGSEARCH)
        {
            printf("Scope.search(%p, '%s' flags=x%x)\n", &this, ident.toChars(), flags);
            // Print scope chain
            for (Scope* sc = &this; sc; sc = sc.enclosing)
            {
                if (!sc.scopesym)
                    continue;
                printf("\tscope %s\n", sc.scopesym.toChars());
            }

            static void printMsg(string txt, Dsymbol s)
            {
                printf("%.*s  %s.%s, kind = '%s'\n", cast(int)txt.length, txt.ptr,
                    s.parent ? s.parent.toChars() : "", s.toChars(), s.kind());
            }
        }

        // This function is called only for unqualified lookup
        assert(!(flags & (SearchLocalsOnly | SearchImportsOnly)));

        /* If ident is "start at module scope", only look at module scope
         */
        if (ident == Id.empty)
        {
            // Look for module scope
            for (Scope* sc = &this; sc; sc = sc.enclosing)
            {
                assert(sc != sc.enclosing);
                if (!sc.scopesym)
                    continue;
                if (Dsymbol s = sc.scopesym.isModule())
                {
                    //printMsg("\tfound", s);
                    if (pscopesym)
                        *pscopesym = sc.scopesym;
                    return s;
                }
            }
            return null;
        }

        Dsymbol checkAliasThis(AggregateDeclaration ad, Identifier ident, int flags, Expression* exp)
        {
            import dmd.mtype;
            if (!ad || !ad.aliasthis)
                return null;

            Declaration decl = ad.aliasthis.sym.isDeclaration();
            if (!decl)
                return null;

            Type t = decl.type;
            ScopeDsymbol sds;
            TypeClass tc;
            TypeStruct ts;
            switch(t.ty)
            {
                case Tstruct:
                    ts = cast(TypeStruct)t;
                    sds = ts.sym;
                    break;
                case Tclass:
                    tc = cast(TypeClass)t;
                    sds = tc.sym;
                    break;
                case Tinstance:
                    sds = (cast(TypeInstance)t).tempinst;
                    break;
                case Tenum:
                    sds = (cast(TypeEnum)t).sym;
                    break;
                default: break;
            }

            if (!sds)
                return null;

            Dsymbol ret = sds.search(loc, ident, flags);
            if (ret)
            {
                *exp = new DotIdExp(loc, *exp, ad.aliasthis.ident);
                *exp = new DotIdExp(loc, *exp, ident);
                return ret;
            }

            if (!ts && !tc)
                return null;

            Dsymbol s;
            *exp = new DotIdExp(loc, *exp, ad.aliasthis.ident);
            if (ts && !(ts.att & AliasThisRec.tracing))
            {
                ts.att = cast(AliasThisRec)(ts.att | AliasThisRec.tracing);
                s = checkAliasThis(sds.isAggregateDeclaration(), ident, flags, exp);
                ts.att = cast(AliasThisRec)(ts.att & ~AliasThisRec.tracing);
            }
            else if(tc && !(tc.att & AliasThisRec.tracing))
            {
                tc.att = cast(AliasThisRec)(tc.att | AliasThisRec.tracing);
                s = checkAliasThis(sds.isAggregateDeclaration(), ident, flags, exp);
                tc.att = cast(AliasThisRec)(tc.att & ~AliasThisRec.tracing);
            }
            return s;
        }

        Dsymbol searchScopes(int flags)
        {
            for (Scope* sc = &this; sc; sc = sc.enclosing)
            {
                assert(sc != sc.enclosing);
                if (!sc.scopesym)
                    continue;
                //printf("\tlooking in scopesym '%s', kind = '%s', flags = x%x\n", sc.scopesym.toChars(), sc.scopesym.kind(), flags);

                if (sc.scopesym.isModule())
                    flags |= SearchUnqualifiedModule;        // tell Module.search() that SearchLocalsOnly is to be obeyed

                if (Dsymbol s = sc.scopesym.search(loc, ident, flags))
                {
                    if (flags & TagNameSpace)
                    {
                        // ImportC: if symbol is not a tag, look for it in tag table
                        if (!s.isScopeDsymbol())
                        {
                            auto ps = cast(void*)s in sc._module.tagSymTab;
                            if (!ps)
                                goto NotFound;
                            s = *ps;
                        }
                    }
                    if (!(flags & (SearchImportsOnly | IgnoreErrors)) &&
                        ident == Id.length && sc.scopesym.isArrayScopeSymbol() &&
                        sc.enclosing && sc.enclosing.search(loc, ident, null, flags))
                    {
                        warning(s.loc, "array `length` hides other `length` name in outer scope");
                    }
                    //printMsg("\tfound local", s);
                    if (pscopesym)
                        *pscopesym = sc.scopesym;
                    return s;
                }

            NotFound:
                if (global.params.fixAliasThis)
                {
                    Expression exp = new ThisExp(loc);
                    Dsymbol aliasSym = checkAliasThis(sc.scopesym.isAggregateDeclaration(), ident, flags, &exp);
                    if (aliasSym)
                    {
                        //printf("found aliassym: %s\n", aliasSym.toChars());
                        if (pscopesym)
                            *pscopesym = new ExpressionDsymbol(exp);
                        return aliasSym;
                    }
                }

                // Stop when we hit a module, but keep going if that is not just under the global scope
                if (sc.scopesym.isModule() && !(sc.enclosing && !sc.enclosing.enclosing))
                    break;
            }
            return null;
        }

        if (this.flags & SCOPE.ignoresymbolvisibility)
            flags |= IgnoreSymbolVisibility;

        // First look in local scopes
        Dsymbol s = searchScopes(flags | SearchLocalsOnly);
        version (LOGSEARCH) if (s) printMsg("-Scope.search() found local", s);
        if (!s)
        {
            // Second look in imported modules
            s = searchScopes(flags | SearchImportsOnly);
            version (LOGSEARCH) if (s) printMsg("-Scope.search() found import", s);
        }
        return s;
    }

    extern (D) Dsymbol search_correct(Identifier ident)
    {
        if (global.gag)
            return null; // don't do it for speculative compiles; too time consuming

        /************************************************
         * Given the failed search attempt, try to find
         * one with a close spelling.
         * Params:
         *      seed = identifier to search for
         *      cost = set to the cost, which rises with each outer scope
         * Returns:
         *      Dsymbol if found, null if not
         */
        extern (D) Dsymbol scope_search_fp(const(char)[] seed, out int cost)
        {
            //printf("scope_search_fp('%s')\n", seed);
            /* If not in the lexer's string table, it certainly isn't in the symbol table.
             * Doing this first is a lot faster.
             */
            if (!seed.length)
                return null;
            Identifier id = Identifier.lookup(seed);
            if (!id)
                return null;
            Scope* sc = &this;
            Module.clearCache();
            Dsymbol scopesym = null;
            Dsymbol s = sc.search(Loc.initial, id, &scopesym, IgnoreErrors);
            if (!s)
                return null;

            // Do not show `@disable`d declarations
            if (auto decl = s.isDeclaration())
                if (decl.storage_class & STC.disable)
                    return null;
            // Or `deprecated` ones if we're not in a deprecated scope
            if (s.isDeprecated() && !sc.isDeprecated())
                return null;

            for (cost = 0; sc; sc = sc.enclosing, ++cost)
                if (sc.scopesym == scopesym)
                    break;
            if (scopesym != s.parent)
            {
                ++cost; // got to the symbol through an import
                if (s.visible().kind == Visibility.Kind.private_)
                    return null;
            }
            return s;
        }

        Dsymbol scopesym = null;
        // search for exact name first
        if (auto s = search(Loc.initial, ident, &scopesym, IgnoreErrors))
            return s;
        return speller!scope_search_fp(ident.toString());
    }

    /************************************
     * Maybe `ident` was a C or C++ name. Check for that,
     * and suggest the D equivalent.
     * Params:
     *  ident = unknown identifier
     * Returns:
     *  D identifier string if found, null if not
     */
    extern (D) static const(char)* search_correct_C(Identifier ident)
    {
        import dmd.astenums : Twchar;
        TOK tok;
        if (ident == Id.NULL)
            tok = TOK.null_;
        else if (ident == Id.TRUE)
            tok = TOK.true_;
        else if (ident == Id.FALSE)
            tok = TOK.false_;
        else if (ident == Id.unsigned)
            tok = TOK.uns32;
        else if (ident == Id.wchar_t)
            tok = target.c.wchar_tsize == 2 ? TOK.wchar_ : TOK.dchar_;
        else
            return null;
        return Token.toChars(tok);
    }

    /***************************
     * Find the innermost scope with a symbol table.
     * Returns:
     *  innermost scope, null if none
     */
    extern (D) Scope* inner() return
    {
        for (Scope* sc = &this; sc; sc = sc.enclosing)
        {
            if (sc.scopesym)
                return sc;
        }
        return null;
    }

    /******************************
     * Add symbol s to innermost symbol table.
     * Params:
     *  s = symbol to insert
     * Returns:
     *  null if already in table, `s` if not
     */
    extern (D) Dsymbol insert(Dsymbol s)
    {
        //printf("insert() %s\n", s.toChars());
        if (VarDeclaration vd = s.isVarDeclaration())
        {
            if (lastVar)
                vd.lastVar = lastVar;
            lastVar = vd;
        }
        else if (WithScopeSymbol ss = s.isWithScopeSymbol())
        {
            if (VarDeclaration vd = ss.withstate.wthis)
            {
                if (lastVar)
                    vd.lastVar = lastVar;
                lastVar = vd;
            }
            return null;
        }

        auto scopesym = inner().scopesym;
        //printf("\t\tscopesym = %p\n", scopesym);
        if (!scopesym.symtab)
            scopesym.symtab = new DsymbolTable();
        if (!(flags & SCOPE.Cfile))
            return scopesym.symtabInsert(s);

        // ImportC insert
        if (!scopesym.symtabInsert(s)) // if already in table
        {
            Dsymbol s2 = scopesym.symtabLookup(s, s.ident); // s2 is existing entry
            return handleTagSymbols(this, s, s2, scopesym);
        }
        return s; // inserted
    }

    /********************************************
     * Search enclosing scopes for ScopeDsymbol.
     */
    ScopeDsymbol getScopesym()
    {
        for (Scope* sc = &this; sc; sc = sc.enclosing)
        {
            if (sc.scopesym)
                return sc.scopesym;
        }
        return null; // not found
    }

    /********************************************
     * Search enclosing scopes for ClassDeclaration.
     */
    extern (C++) ClassDeclaration getClassScope()
    {
        for (Scope* sc = &this; sc; sc = sc.enclosing)
        {
            if (!sc.scopesym)
                continue;
            if (ClassDeclaration cd = sc.scopesym.isClassDeclaration())
                return cd;
        }
        return null;
    }

    /********************************************
     * Search enclosing scopes for ClassDeclaration.
     */
    extern (C++) AggregateDeclaration getStructClassScope()
    {
        for (Scope* sc = &this; sc; sc = sc.enclosing)
        {
            if (!sc.scopesym)
                continue;
            if (AggregateDeclaration ad = sc.scopesym.isClassDeclaration())
                return ad;
            if (AggregateDeclaration ad = sc.scopesym.isStructDeclaration())
                return ad;
        }
        return null;
    }

    /*******************************************
     * For TemplateDeclarations, we need to remember the Scope
     * where it was declared. So mark the Scope as not
     * to be free'd.
     */
    extern (D) void setNoFree()
    {
        //int i = 0;
        //printf("Scope::setNoFree(this = %p)\n", this);
        for (Scope* sc = &this; sc; sc = sc.enclosing)
        {
            //printf("\tsc = %p\n", sc);
            sc.nofree = true;
            assert(!(flags & SCOPE.free));
            //assert(sc != sc.enclosing);
            //assert(!sc.enclosing || sc != sc.enclosing.enclosing);
            //if (++i == 10)
            //    assert(0);
        }
    }

    structalign_t alignment()
    {
        if (aligndecl)
            return aligndecl.getAlignment(&this);
        else
            return STRUCTALIGN_DEFAULT;
    }

    /**********************************
    * Checks whether the current scope (or any of its parents) is deprecated.
    *
    * Returns: `true` if this or any parent scope is deprecated, `false` otherwise`
    */
    extern(C++) bool isDeprecated() @safe @nogc pure nothrow const
    {
        for (const(Dsymbol)* sp = &(this.parent); *sp; sp = &(sp.parent))
        {
            if (sp.isDeprecated())
                return true;
        }
        for (const(Scope)* sc2 = &this; sc2; sc2 = sc2.enclosing)
        {
            if (sc2.scopesym && sc2.scopesym.isDeprecated())
                return true;

            // If inside a StorageClassDeclaration that is deprecated
            if (sc2.stc & STC.deprecated_)
                return true;
        }
        if (_module.md && _module.md.isdeprecated)
        {
            return true;
        }
        return false;
    }
}
