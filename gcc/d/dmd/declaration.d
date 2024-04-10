/**
 * Miscellaneous declarations, including typedef, alias, variable declarations including the
 * implicit this declaration, type tuples, ClassInfo, ModuleInfo and various TypeInfos.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/declaration.d, _declaration.d)
 * Documentation:  https://dlang.org/phobos/dmd_declaration.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/declaration.d
 */

module dmd.declaration;

import core.stdc.stdio;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ctorflow;
import dmd.dclass;
import dmd.delegatize;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.gluelayer;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.initsem;
import dmd.intrange;
import dmd.location;
import dmd.mtype;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

version (IN_GCC) {}
else version (IN_LLVM) {}
else version = MARS;

/************************************
 * Check to see the aggregate type is nested and its context pointer is
 * accessible from the current scope.
 * Returns true if error occurs.
 */
bool checkFrameAccess(Loc loc, Scope* sc, AggregateDeclaration ad, size_t iStart = 0)
{
    Dsymbol sparent = ad.toParentLocal();
    Dsymbol sparent2 = ad.toParent2();
    Dsymbol s = sc.func;
    if (ad.isNested() && s)
    {
        //printf("ad = %p %s [%s], parent:%p\n", ad, ad.toChars(), ad.loc.toChars(), ad.parent);
        //printf("sparent = %p %s [%s], parent: %s\n", sparent, sparent.toChars(), sparent.loc.toChars(), sparent.parent,toChars());
        //printf("sparent2 = %p %s [%s], parent: %s\n", sparent2, sparent2.toChars(), sparent2.loc.toChars(), sparent2.parent,toChars());
        if (!ensureStaticLinkTo(s, sparent) || sparent != sparent2 && !ensureStaticLinkTo(s, sparent2))
        {
            error(loc, "cannot access frame pointer of `%s`", ad.toPrettyChars());
            return true;
        }
    }

    bool result = false;
    for (size_t i = iStart; i < ad.fields.length; i++)
    {
        VarDeclaration vd = ad.fields[i];
        Type tb = vd.type.baseElemOf();
        if (tb.ty == Tstruct)
        {
            result |= checkFrameAccess(loc, sc, (cast(TypeStruct)tb).sym);
        }
    }
    return result;
}

/***********************************************
 * Mark variable v as modified if it is inside a constructor that var
 * is a field in.
 * Also used to allow immutable globals to be initialized inside a static constructor.
 * Returns:
 *    true if it's an initialization of v
 */
bool modifyFieldVar(Loc loc, Scope* sc, VarDeclaration var, Expression e1)
{
    //printf("modifyFieldVar(var = %s)\n", var.toChars());
    Dsymbol s = sc.func;
    while (1)
    {
        FuncDeclaration fd = null;
        if (s)
            fd = s.isFuncDeclaration();
        if (fd &&
            ((fd.isCtorDeclaration() && var.isField()) ||
             ((fd.isStaticCtorDeclaration() || fd.isCrtCtor) && !var.isField())) &&
            fd.toParentDecl() == var.toParent2() &&
            (!e1 || e1.op == EXP.this_))
        {
            bool result = true;

            var.ctorinit = true;
            //printf("setting ctorinit\n");

            if (var.isField() && sc.ctorflow.fieldinit.length && !sc.intypeof)
            {
                assert(e1);
                auto mustInit = ((var.storage_class & STC.nodefaultctor) != 0 ||
                                 var.type.needsNested());

                const dim = sc.ctorflow.fieldinit.length;
                auto ad = fd.isMemberDecl();
                assert(ad);
                size_t i;
                for (i = 0; i < dim; i++) // same as findFieldIndexByName in ctfeexp.c ?
                {
                    if (ad.fields[i] == var)
                        break;
                }
                assert(i < dim);
                auto fieldInit = &sc.ctorflow.fieldinit[i];
                const fi = fieldInit.csx;

                if (fi & CSX.this_ctor)
                {
                    if (var.type.isMutable() && e1.type.isMutable())
                        result = false;
                    else
                    {
                        const(char)* modStr = !var.type.isMutable() ? MODtoChars(var.type.mod) : MODtoChars(e1.type.mod);
                        .error(loc, "%s field `%s` initialized multiple times", modStr, var.toChars());
                        .errorSupplemental(fieldInit.loc, "Previous initialization is here.");
                    }
                }
                else if (sc.inLoop || (fi & CSX.label))
                {
                    if (!mustInit && var.type.isMutable() && e1.type.isMutable())
                        result = false;
                    else
                    {
                        const(char)* modStr = !var.type.isMutable() ? MODtoChars(var.type.mod) : MODtoChars(e1.type.mod);
                        .error(loc, "%s field `%s` initialization is not allowed in loops or after labels", modStr, var.toChars());
                    }
                }

                fieldInit.csx |= CSX.this_ctor;
                fieldInit.loc = e1.loc;
                if (var.overlapped) // https://issues.dlang.org/show_bug.cgi?id=15258
                {
                    foreach (j, v; ad.fields)
                    {
                        if (v is var || !var.isOverlappedWith(v))
                            continue;
                        v.ctorinit = true;
                        sc.ctorflow.fieldinit[j].csx = CSX.this_ctor;
                    }
                }
            }
            else if (fd != sc.func)
            {
                if (var.type.isMutable())
                    result = false;
                else if (sc.func.fes)
                {
                    const(char)* p = var.isField() ? "field" : var.kind();
                    .error(loc, "%s %s `%s` initialization is not allowed in foreach loop",
                        MODtoChars(var.type.mod), p, var.toChars());
                }
                else
                {
                    const(char)* p = var.isField() ? "field" : var.kind();
                    .error(loc, "%s %s `%s` initialization is not allowed in nested function `%s`",
                        MODtoChars(var.type.mod), p, var.toChars(), sc.func.toChars());
                }
            }
            else if (fd.isStaticCtorDeclaration() && !fd.isSharedStaticCtorDeclaration() &&
                     var.type.isImmutable())
            {
                .error(loc, "%s %s `%s` initialization is not allowed in `static this`",
                    MODtoChars(var.type.mod), var.kind(), var.toChars());
                errorSupplemental(loc, "Use `shared static this` instead.");
            }
            else if (fd.isStaticCtorDeclaration() && !fd.isSharedStaticCtorDeclaration() &&
                    var.type.isConst())
            {
                // @@@DEPRECATED_2.116@@@
                // Turn this into an error, merging with the branch above
                .deprecation(loc, "%s %s `%s` initialization is not allowed in `static this`",
                    MODtoChars(var.type.mod), var.kind(), var.toChars());
                deprecationSupplemental(loc, "Use `shared static this` instead.");
            }
            return result;
        }
        else
        {
            if (s)
            {
                s = s.toParentP(var.toParent2());
                continue;
            }
        }
        break;
    }
    return false;
}

/******************************************
 */
void ObjectNotFound(Identifier id)
{
    error(Loc.initial, "`%s` not found. object.d may be incorrectly installed or corrupt.", id.toChars());
    fatal();
}

/* Accumulator for successive matches.
 */
struct MatchAccumulator
{
    int count;              // number of matches found so far
    MATCH last = MATCH.nomatch; // match level of lastf
    FuncDeclaration lastf;  // last matching function we found
    FuncDeclaration nextf;  // if ambiguous match, this is the "other" function
}

/***********************************************************
 */
extern (C++) abstract class Declaration : Dsymbol
{
    Type type;
    Type originalType;  // before semantic analysis
    StorageClass storage_class = STC.undefined_;
    Visibility visibility;
    LINK _linkage = LINK.default_; // may be `LINK.system`; use `resolvedLinkage()` to resolve it
    short inuse;          // used to detect cycles

    ubyte adFlags;         // control re-assignment of AliasDeclaration (put here for packing reasons)
      enum wasRead    = 1; // set if AliasDeclaration was read
      enum ignoreRead = 2; // ignore any reads of AliasDeclaration
      enum nounderscore = 4; // don't prepend _ to mangled name
      enum hidden       = 8; // don't print this in .di files

    // overridden symbol with pragma(mangle, "...")
    const(char)[] mangleOverride;

    final extern (D) this(Identifier ident) @safe
    {
        super(ident);
        visibility = Visibility(Visibility.Kind.undefined);
    }

    final extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, ident);
        visibility = Visibility(Visibility.Kind.undefined);
    }

    override const(char)* kind() const
    {
        return "declaration";
    }

    override final uinteger_t size(const ref Loc loc)
    {
        assert(type);
        const sz = type.size();
        if (sz == SIZE_INVALID)
            errors = true;
        return sz;
    }

    /**
     * Issue an error if an attempt to call a disabled method is made
     *
     * If the declaration is disabled but inside a disabled function,
     * returns `true` but do not issue an error message.
     *
     * Params:
     *   loc = Location information of the call
     *   sc  = Scope in which the call occurs
     *   isAliasedDeclaration = if `true` searches overload set
     *
     * Returns:
     *   `true` if this `Declaration` is `@disable`d, `false` otherwise.
     */
    extern (D) final bool checkDisabled(Loc loc, Scope* sc, bool isAliasedDeclaration = false)
    {
        if (!(storage_class & STC.disable))
            return false;

        if (sc.func && sc.func.storage_class & STC.disable)
            return true;

        if (auto p = toParent())
        {
            if (auto postblit = isPostBlitDeclaration())
            {
                /* https://issues.dlang.org/show_bug.cgi?id=21885
                 *
                 * If the generated postblit is disabled, it
                 * means that one of the fields has a disabled
                 * postblit. Print the first field that has
                 * a disabled postblit.
                 */
                if (postblit.isGenerated())
                {
                    auto sd = p.isStructDeclaration();
                    assert(sd);
                    for (size_t i = 0; i < sd.fields.length; i++)
                    {
                        auto structField = sd.fields[i];
                        if (structField.overlapped)
                            continue;
                        Type tv = structField.type.baseElemOf();
                        if (tv.ty != Tstruct)
                            continue;
                        auto sdv = (cast(TypeStruct)tv).sym;
                        if (!sdv.postblit)
                            continue;
                        if (sdv.postblit.isDisabled())
                        {
                            .error(loc, "%s `%s` is not copyable because field `%s` is not copyable", p.kind, p.toPrettyChars, structField.toChars());
                            return true;
                        }
                    }
                }
                .error(loc, "%s `%s` is not copyable because it has a disabled postblit", p.kind, p.toPrettyChars);
                return true;
            }
        }

        // if the function is @disabled, maybe there
        // is an overload in the overload set that isn't
        if (isAliasedDeclaration)
        {
            FuncDeclaration fd = isFuncDeclaration();
            if (fd)
            {
                for (FuncDeclaration ovl = fd; ovl; ovl = cast(FuncDeclaration)ovl.overnext)
                    if (!(ovl.storage_class & STC.disable))
                        return false;
            }
        }

        if (auto ctor = isCtorDeclaration())
        {
            if (ctor.isCpCtor && ctor.isGenerated())
            {
                .error(loc, "generating an `inout` copy constructor for `struct %s` failed, therefore instances of it are uncopyable", parent.toPrettyChars());
                return true;
            }
        }
        .error(loc, "%s `%s` cannot be used because it is annotated with `@disable`", kind, toPrettyChars);
        return true;
    }

    /*************************************
     * Check to see if declaration can be modified in this context (sc).
     * Issue error if not.
     * Params:
     *  loc  = location for error messages
     *  e1   = `null` or `this` expression when this declaration is a field
     *  sc   = context
     *  flag = if the first bit is set it means do not issue error message for
     *         invalid modification; if the second bit is set, it means that
               this declaration is a field and a subfield of it is modified.
     * Returns:
     *  Modifiable.yes or Modifiable.initialization
     */
    extern (D) final Modifiable checkModify(Loc loc, Scope* sc, Expression e1, ModifyFlags flag)
    {
        VarDeclaration v = isVarDeclaration();
        if (v && v.canassign)
            return Modifiable.initialization;

        if (isParameter() || isResult())
        {
            for (Scope* scx = sc; scx; scx = scx.enclosing)
            {
                if (scx.func == parent && (scx.flags & SCOPE.contract))
                {
                    const(char)* s = isParameter() && parent.ident != Id.ensure ? "parameter" : "result";
                    if (!(flag & ModifyFlags.noError))
                        error(loc, "%s `%s` cannot modify %s `%s` in contract", kind, toPrettyChars, s, toChars());
                    return Modifiable.initialization; // do not report type related errors
                }
            }
        }

        if (e1 && e1.op == EXP.this_ && isField())
        {
            VarDeclaration vthis = e1.isThisExp().var;
            for (Scope* scx = sc; scx; scx = scx.enclosing)
            {
                if (scx.func == vthis.parent && (scx.flags & SCOPE.contract))
                {
                    if (!(flag & ModifyFlags.noError))
                        error(loc, "%s `%s` cannot modify parameter `this` in contract", kind, toPrettyChars);
                    return Modifiable.initialization; // do not report type related errors
                }
            }
        }

        if (v && (v.isCtorinit() || isField()))
        {
            // It's only modifiable if inside the right constructor
            if ((storage_class & (STC.foreach_ | STC.ref_)) == (STC.foreach_ | STC.ref_))
                return Modifiable.initialization;
            if (flag & ModifyFlags.fieldAssign)
                return Modifiable.yes;
            return modifyFieldVar(loc, sc, v, e1) ? Modifiable.initialization : Modifiable.yes;
        }
        return Modifiable.yes;
    }

    final bool isStatic() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.static_) != 0;
    }

    /// Returns the linkage, resolving the target-specific `System` one.
    final LINK resolvedLinkage() const
    {
        return _linkage == LINK.system ? target.systemLinkage() : _linkage;
    }

    bool isDelete()
    {
        return false;
    }

    bool isDataseg()
    {
        return false;
    }

    bool isThreadlocal()
    {
        return false;
    }

    bool isCodeseg() const pure nothrow @nogc @safe
    {
        return false;
    }

    final bool isFinal() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.final_) != 0;
    }

    bool isAbstract()
    {
        return (storage_class & STC.abstract_) != 0;
    }

    final bool isConst() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.const_) != 0;
    }

    final bool isImmutable() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.immutable_) != 0;
    }

    final bool isWild() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.wild) != 0;
    }

    final bool isAuto() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.auto_) != 0;
    }

    final bool isScope() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.scope_) != 0;
    }

    final bool isReturn() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.return_) != 0;
    }

    final bool isSynchronized() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.synchronized_) != 0;
    }

    final bool isParameter() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.parameter) != 0;
    }

    override final bool isDeprecated() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.deprecated_) != 0;
    }

    final bool isDisabled() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.disable) != 0;
    }

    final bool isOverride() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.override_) != 0;
    }

    final bool isResult() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.result) != 0;
    }

    final bool isField() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.field) != 0;
    }

    final bool isIn() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.in_) != 0;
    }

    final bool isOut() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.out_) != 0;
    }

    final bool isRef() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.ref_) != 0;
    }

    /// Returns: Whether the variable is a reference, annotated with `out` or `ref`
    final bool isReference() const pure nothrow @nogc @safe
    {
        return (storage_class & (STC.ref_ | STC.out_)) != 0;
    }

    final bool isFuture() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.future) != 0;
    }

    final extern(D) bool isSystem() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.system) != 0;
    }

    override final Visibility visible() pure nothrow @nogc @safe
    {
        return visibility;
    }

    override final inout(Declaration) isDeclaration() inout pure nothrow @nogc @safe
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
extern (C++) final class TupleDeclaration : Declaration
{
    Objects* objects;
    TypeTuple tupletype;    // !=null if this is a type tuple
    bool isexp;             // true: expression tuple
    bool building;          // it's growing in AliasAssign semantic

    extern (D) this(const ref Loc loc, Identifier ident, Objects* objects) @safe
    {
        super(loc, ident);
        this.objects = objects;
    }

    override TupleDeclaration syntaxCopy(Dsymbol s)
    {
        assert(0);
    }

    override const(char)* kind() const
    {
        return "sequence";
    }

    override Type getType()
    {
        /* If this tuple represents a type, return that type
         */

        //printf("TupleDeclaration::getType() %s\n", toChars());
        if (isexp || building)
            return null;
        if (!tupletype)
        {
            /* It's only a type tuple if all the Object's are types
             */
            for (size_t i = 0; i < objects.length; i++)
            {
                RootObject o = (*objects)[i];
                if (!o.isType())
                {
                    //printf("\tnot[%d], %p, %d\n", i, o, o.dyncast());
                    return null;
                }
            }

            /* We know it's a type tuple, so build the TypeTuple
             */
            Types* types = cast(Types*)objects;
            auto args = new Parameters(objects.length);
            OutBuffer buf;
            int hasdeco = 1;
            for (size_t i = 0; i < types.length; i++)
            {
                Type t = (*types)[i];
                //printf("type = %s\n", t.toChars());
                version (none)
                {
                    buf.printf("_%s_%d", ident.toChars(), i);
                    auto id = Identifier.idPool(buf.extractSlice());
                    auto arg = new Parameter(Loc.initial, STC.in_, t, id, null);
                }
                else
                {
                    auto arg = new Parameter(Loc.initial, 0, t, null, null, null);
                }
                (*args)[i] = arg;
                if (!t.deco)
                    hasdeco = 0;
            }

            tupletype = new TypeTuple(args);
            if (hasdeco)
                return tupletype.typeSemantic(Loc.initial, null);
        }
        return tupletype;
    }

    override Dsymbol toAlias2()
    {
        //printf("TupleDeclaration::toAlias2() '%s' objects = %s\n", toChars(), objects.toChars());
        for (size_t i = 0; i < objects.length; i++)
        {
            RootObject o = (*objects)[i];
            if (Dsymbol s = isDsymbol(o))
            {
                s = s.toAlias2();
                (*objects)[i] = s;
            }
        }
        return this;
    }

    override bool needThis()
    {
        //printf("TupleDeclaration::needThis(%s)\n", toChars());
        return isexp ? foreachVar((s) { return s.needThis(); }) != 0 : false;
    }

    /***********************************************************
     * Calls dg(Dsymbol) for each Dsymbol, which should be a VarDeclaration
     * inside VarExp (isexp == true).
     * Params:
     *    dg = delegate to call for each Dsymbol
     */
    extern (D) void foreachVar(scope void delegate(Dsymbol) dg)
    {
        assert(isexp);
        foreach (o; *objects)
        {
            if (auto e = o.isExpression())
                if (auto ve = e.isVarExp())
                    dg(ve.var);
        }
    }

    /***********************************************************
     * Calls dg(Dsymbol) for each Dsymbol, which should be a VarDeclaration
     * inside VarExp (isexp == true).
     * If dg returns !=0, stops and returns that value else returns 0.
     * Params:
     *    dg = delegate to call for each Dsymbol
     * Returns:
     *    last value returned by dg()
     */
    extern (D) int foreachVar(scope int delegate(Dsymbol) dg)
    {
        assert(isexp);
        foreach (o; *objects)
        {
            if (auto e = o.isExpression())
                if (auto ve = e.isVarExp())
                    if(auto ret = dg(ve.var))
                        return ret;
        }
        return 0;
    }

    override inout(TupleDeclaration) isTupleDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/declaration.html#AliasDeclaration
 */
extern (C++) final class AliasDeclaration : Declaration
{
    Dsymbol aliassym;   // alias ident = aliassym;

    Dsymbol overnext;   // next in overload list
    Dsymbol _import;    // !=null if unresolved internal alias for selective import

    extern (D) this(const ref Loc loc, Identifier ident, Type type) @safe
    {
        super(loc, ident);
        //printf("AliasDeclaration(id = '%s', type = %p)\n", ident.toChars(), type);
        //printf("type = '%s'\n", type.toChars());
        this.type = type;
        assert(type);
    }

    extern (D) this(const ref Loc loc, Identifier ident, Dsymbol s) @safe
    {
        super(loc, ident);
        //printf("AliasDeclaration(id = '%s', s = %p)\n", ident.toChars(), s);
        assert(s != this);
        this.aliassym = s;
        assert(s);
    }

    static AliasDeclaration create(const ref Loc loc, Identifier id, Type type) @safe
    {
        return new AliasDeclaration(loc, id, type);
    }

    override AliasDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("AliasDeclaration::syntaxCopy()\n");
        assert(!s);
        AliasDeclaration sa = type ? new AliasDeclaration(loc, ident, type.syntaxCopy()) : new AliasDeclaration(loc, ident, aliassym.syntaxCopy(null));
        sa.comment = comment;
        sa.storage_class = storage_class;
        return sa;
    }

    override bool overloadInsert(Dsymbol s)
    {
        //printf("[%s] AliasDeclaration::overloadInsert('%s') s = %s %s @ [%s]\n",
        //       loc.toChars(), toChars(), s.kind(), s.toChars(), s.loc.toChars());

        /** Aliases aren't overloadable themselves, but if their Aliasee is
         *  overloadable they are converted to an overloadable Alias (either
         *  FuncAliasDeclaration or OverDeclaration).
         *
         *  This is done by moving the Aliasee into such an overloadable alias
         *  which is then used to replace the existing Aliasee. The original
         *  Alias (_this_) remains a useless shell.
         *
         *  This is a horrible mess. It was probably done to avoid replacing
         *  existing AST nodes and references, but it needs a major
         *  simplification b/c it's too complex to maintain.
         *
         *  A simpler approach might be to merge any colliding symbols into a
         *  simple Overload class (an array) and then later have that resolve
         *  all collisions.
         */
        if (semanticRun >= PASS.semanticdone)
        {
            /* Semantic analysis is already finished, and the aliased entity
             * is not overloadable.
             */
            if (type)
            {
                /*
                    If type has been resolved already we could
                    still be inserting an alias from an import.

                    If we are handling an alias then pretend
                    it was inserting and return true, if not then
                    false since we didn't even pretend to insert something.
                */
                return this._import && this.equals(s);
            }

            // https://issues.dlang.org/show_bug.cgi?id=23865
            // only insert if the symbol can be part of a set
            const s1 = s.toAlias();
            const isInsertCandidate = s1.isFuncDeclaration() || s1.isOverDeclaration() || s1.isTemplateDeclaration();

            /* When s is added in member scope by static if, mixin("code") or others,
             * aliassym is determined already. See the case in: test/compilable/test61.d
             */
            auto sa = aliassym.toAlias();

            if (auto td = s.toAlias().isTemplateDeclaration())
                s = td.funcroot ? td.funcroot : td;

            if (auto fd = sa.isFuncDeclaration())
            {
                auto fa = new FuncAliasDeclaration(ident, fd);
                fa.visibility = visibility;
                fa.parent = parent;
                aliassym = fa;
                if (isInsertCandidate)
                    return aliassym.overloadInsert(s);
            }
            if (auto td = sa.isTemplateDeclaration())
            {
                auto od = new OverDeclaration(ident, td.funcroot ? td.funcroot : td);
                od.visibility = visibility;
                od.parent = parent;
                aliassym = od;
                if (isInsertCandidate)
                    return aliassym.overloadInsert(s);
            }
            if (auto od = sa.isOverDeclaration())
            {
                if (sa.ident != ident || sa.parent != parent)
                {
                    od = new OverDeclaration(ident, od);
                    od.visibility = visibility;
                    od.parent = parent;
                    aliassym = od;
                }
                if (isInsertCandidate)
                    return od.overloadInsert(s);
            }
            if (auto os = sa.isOverloadSet())
            {
                if (sa.ident != ident || sa.parent != parent)
                {
                    os = new OverloadSet(ident, os);
                    // TODO: visibility is lost here b/c OverloadSets have no visibility attribute
                    // Might no be a practical issue, b/c the code below fails to resolve the overload anyhow.
                    // ----
                    // module os1;
                    // import a, b;
                    // private alias merged = foo; // private alias to overload set of a.foo and b.foo
                    // ----
                    // module os2;
                    // import a, b;
                    // public alias merged = bar; // public alias to overload set of a.bar and b.bar
                    // ----
                    // module bug;
                    // import os1, os2;
                    // void test() { merged(123); } // should only look at os2.merged
                    //
                    // os.visibility = visibility;
                    os.parent = parent;
                    aliassym = os;
                }
                if (isInsertCandidate)
                {
                    os.push(s);
                    return true;
                }
            }
            return false;
        }

        /* Don't know yet what the aliased symbol is, so assume it can
         * be overloaded and check later for correctness.
         */
        if (overnext)
            return overnext.overloadInsert(s);
        if (s is this)
            return true;
        overnext = s;
        return true;
    }

    override const(char)* kind() const
    {
        return "alias";
    }

    override Type getType()
    {
        if (type)
            return type;
        return toAlias().getType();
    }

    override Dsymbol toAlias()
    {
        //printf("[%s] AliasDeclaration::toAlias('%s', this = %p, aliassym = %p, kind = '%s', inuse = %d)\n",
        //    loc.toChars(), toChars(), this, aliassym, aliassym ? aliassym.kind() : "", inuse);
        assert(this != aliassym);
        //static int count; if (++count == 10) *(char*)0=0;

        // Reading the AliasDeclaration
        if (!(adFlags & ignoreRead))
            adFlags |= wasRead;                 // can never assign to this AliasDeclaration again

        if (inuse == 1 && type && _scope)
        {
            inuse = 2;
            uint olderrors = global.errors;
            Dsymbol s = type.toDsymbol(_scope);
            //printf("[%s] type = %s, s = %p, this = %p\n", loc.toChars(), type.toChars(), s, this);
            if (global.errors != olderrors)
                goto Lerr;
            if (s)
            {
                s = s.toAlias();
                if (global.errors != olderrors)
                    goto Lerr;
                aliassym = s;
                inuse = 0;
            }
            else
            {
                Type t = type.typeSemantic(loc, _scope);
                if (t.ty == Terror)
                    goto Lerr;
                if (global.errors != olderrors)
                    goto Lerr;
                //printf("t = %s\n", t.toChars());
                inuse = 0;
            }
        }
        if (inuse)
        {
            .error(loc, "%s `%s` recursive alias declaration", kind, toPrettyChars);

        Lerr:
            // Avoid breaking "recursive alias" state during errors gagged
            if (global.gag)
                return this;
            aliassym = new AliasDeclaration(loc, ident, Type.terror);
            type = Type.terror;
            return aliassym;
        }

        if (semanticRun >= PASS.semanticdone)
        {
            // semantic is already done.

            // Do not see aliassym !is null, because of lambda aliases.

            // Do not see type.deco !is null, even so "alias T = const int;` needs
            // semantic analysis to take the storage class `const` as type qualifier.
        }
        else
        {
            // stop AliasAssign tuple building
            if (aliassym)
            {
                if (auto td = aliassym.isTupleDeclaration())
                {
                    if (td.building)
                    {
                        td.building = false;
                        semanticRun = PASS.semanticdone;
                        return td;
                    }
                }
            }
            if (_import && _import._scope)
            {
                /* If this is an internal alias for selective/renamed import,
                 * load the module first.
                 */
                _import.dsymbolSemantic(null);
            }
            if (_scope)
            {
                aliasSemantic(this, _scope);
            }
        }

        inuse = 1;
        Dsymbol s = aliassym ? aliassym.toAlias() : this;
        inuse = 0;
        return s;
    }

    override Dsymbol toAlias2()
    {
        if (inuse)
        {
            .error(loc, "%s `%s` recursive alias declaration", kind, toPrettyChars);
            return this;
        }
        inuse = 1;
        Dsymbol s = aliassym ? aliassym.toAlias2() : this;
        inuse = 0;
        return s;
    }

    override bool isOverloadable() const
    {
        // assume overloadable until alias is resolved
        return semanticRun < PASS.semanticdone ||
            aliassym && aliassym.isOverloadable();
    }

    override inout(AliasDeclaration) isAliasDeclaration() inout
    {
        return this;
    }

    /** Returns: `true` if this instance was created to make a template parameter
    visible in the scope of a template body, `false` otherwise */
    extern (D) bool isAliasedTemplateParameter() const
    {
        return !!(storage_class & STC.templateparameter);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class OverDeclaration : Declaration
{
    Dsymbol overnext;   // next in overload list
    Dsymbol aliassym;

    extern (D) this(Identifier ident, Dsymbol s) @safe
    {
        super(ident);
        this.aliassym = s;
    }

    override const(char)* kind() const
    {
        return "overload alias"; // todo
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;

        auto s = isDsymbol(o);
        if (!s)
            return false;

        if (auto od2 = s.isOverDeclaration())
            return this.aliassym.equals(od2.aliassym);
        return this.aliassym == s;
    }

    override bool overloadInsert(Dsymbol s)
    {
        //printf("OverDeclaration::overloadInsert('%s') aliassym = %p, overnext = %p\n", s.toChars(), aliassym, overnext);
        if (overnext)
            return overnext.overloadInsert(s);
        if (s == this)
            return true;
        overnext = s;
        return true;
    }

    override bool isOverloadable() const
    {
        return true;
    }

    Dsymbol isUnique()
    {
        Dsymbol result = null;
        overloadApply(aliassym, (Dsymbol s)
        {
            if (result)
            {
                result = null;
                return 1; // ambiguous, done
            }
            else
            {
                result = s;
                return 0;
            }
        });
        return result;
    }

    override inout(OverDeclaration) isOverDeclaration() inout
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
extern (C++) class VarDeclaration : Declaration
{
    Initializer _init;
    FuncDeclarations nestedrefs;    // referenced by these lexically nested functions
    TupleDeclaration aliasTuple;    // when `this` is really a tuple of declarations
    VarDeclaration lastVar;         // Linked list of variables for goto-skips-init detection
    Expression edtor;               // if !=null, does the destruction of the variable
    IntRange* range;                // if !=null, the variable is known to be within the range
    VarDeclarations* maybes;        // maybeScope variables that are assigned to this maybeScope variable

    uint endlinnum;                 // line number of end of scope that this var lives in
    uint offset;
    uint sequenceNumber;            // order the variables are declared
    structalign_t alignment;

    // When interpreting, these point to the value (NULL if value not determinable)
    // The index of this variable on the CTFE stack, AdrOnStackNone if not allocated
    enum AdrOnStackNone = ~0u;
    uint ctfeAdrOnStack;

    // `bool` fields that are compacted into bit fields in a string mixin
    private extern (D) static struct BitFields
    {
        bool isargptr;          /// if parameter that _argptr points to
        bool ctorinit;          /// it has been initialized in a ctor
        bool iscatchvar;        /// this is the exception object variable in catch() clause
        bool isowner;           /// this is an Owner, despite it being `scope`
        bool setInCtorOnly;     /// field can only be set in a constructor, as it is const or immutable

        /// It is a class that was allocated on the stack
        ///
        /// This means the var is not rebindable once assigned,
        /// and the destructor gets run when it goes out of scope
        bool onstack;

        bool overlapped;        /// if it is a field and has overlapping
        bool overlapUnsafe;     /// if it is an overlapping field and the overlaps are unsafe
        bool maybeScope;        /// allow inferring 'scope' for this variable
        bool doNotInferReturn;  /// do not infer 'return' for this variable

        bool isArgDtorVar;      /// temporary created to handle scope destruction of a function argument
        bool isCmacro;          /// it is a C macro turned into a C declaration
        bool dllImport;         /// __declspec(dllimport)
        bool dllExport;         /// __declspec(dllexport)
        version (MARS)
        {
            bool inClosure;         /// is inserted into a GC allocated closure
            bool inAlignSection;    /// is inserted into an aligned section on stack
        }
        bool systemInferred;    /// @system was inferred from initializer
    }

    import dmd.common.bitfields : generateBitFields;
    mixin(generateBitFields!(BitFields, uint));

    byte canassign;                 // it can be assigned to
    ubyte isdataseg;                // private data for isDataseg 0 unset, 1 true, 2 false

    final extern (D) this(const ref Loc loc, Type type, Identifier ident, Initializer _init, StorageClass storage_class = STC.undefined_)
    in
    {
        assert(ident);
    }
    do
    {
        //printf("VarDeclaration('%s')\n", ident.toChars());
        super(loc, ident);
        debug
        {
            if (!type && !_init)
            {
                //printf("VarDeclaration('%s')\n", ident.toChars());
                //*(char*)0=0;
            }
        }

        assert(type || _init);
        this.type = type;
        this._init = _init;
        ctfeAdrOnStack = AdrOnStackNone;
        this.storage_class = storage_class;
    }

    static VarDeclaration create(const ref Loc loc, Type type, Identifier ident, Initializer _init, StorageClass storage_class = STC.undefined_)
    {
        return new VarDeclaration(loc, type, ident, _init, storage_class);
    }

    override VarDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("VarDeclaration::syntaxCopy(%s)\n", toChars());
        assert(!s);
        auto v = new VarDeclaration(loc, type ? type.syntaxCopy() : null, ident, _init ? _init.syntaxCopy() : null, storage_class);
        v.comment = comment;
        return v;
    }

    override const(char)* kind() const
    {
        return "variable";
    }

    override final inout(AggregateDeclaration) isThis() inout
    {
        if (!(storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.templateparameter | STC.gshared | STC.ctfe)))
        {
            /* The casting is necessary because `s = s.parent` is otherwise rejected
             */
            for (auto s = cast(Dsymbol)this; s; s = s.parent)
            {
                auto ad = (cast(inout)s).isMember();
                if (ad)
                    return ad;
                if (!s.parent || !s.parent.isTemplateMixin())
                    break;
            }
        }
        return null;
    }

    override final bool needThis()
    {
        //printf("VarDeclaration::needThis(%s, x%x)\n", toChars(), storage_class);
        return isField();
    }

    override final bool isExport() const
    {
        return visibility.kind == Visibility.Kind.export_ || dllExport;
    }

    override final bool isImportedSymbol() const
    {
        /* If global variable has `export` and `extern` then it is imported
         *   export int sym1;            // definition:  exported
         *   export extern int sym2;     // declaration: imported
         *   export extern int sym3 = 0; // error, extern cannot have initializer
         */
        bool result =
            dllImport ||
            visibility.kind == Visibility.Kind.export_ &&
            storage_class & STC.extern_ &&
            (storage_class & STC.static_ || parent.isModule());
        //printf("isImportedSymbol() %s %d\n", toChars(), result);
        return result;
    }

    final bool isCtorinit() const pure nothrow @nogc @safe
    {
        return setInCtorOnly;
    }

    /*******************************
     * Does symbol go into data segment?
     * Includes extern variables.
     */
    override final bool isDataseg()
    {
        version (none)
        {
            printf("VarDeclaration::isDataseg(%p, '%s')\n", this, toChars());
            printf("%llx, isModule: %p, isTemplateInstance: %p, isNspace: %p\n",
                   storage_class & (STC.static_ | STC.const_), parent.isModule(), parent.isTemplateInstance(), parent.isNspace());
            printf("parent = '%s'\n", parent.toChars());
        }

        if (isdataseg == 0) // the value is not cached
        {
            isdataseg = 2; // The Variables does not go into the datasegment

            if (!canTakeAddressOf())
            {
                return false;
            }

            Dsymbol parent = toParent();
            if (!parent && !(storage_class & STC.static_))
            {
                .error(loc, "%s `%s` forward referenced", kind, toPrettyChars);
                type = Type.terror;
            }
            else if (storage_class & (STC.static_ | STC.extern_ | STC.gshared) ||
                parent.isModule() || parent.isTemplateInstance() || parent.isNspace())
            {
                assert(!isParameter() && !isResult());
                isdataseg = 1; // It is in the DataSegment
            }
        }

        return (isdataseg == 1);
    }
    /************************************
     * Does symbol go into thread local storage?
     */
    override final bool isThreadlocal()
    {
        //printf("VarDeclaration::isThreadlocal(%p, '%s')\n", this, toChars());
        /* Data defaults to being thread-local. It is not thread-local
         * if it is immutable, const or shared.
         */
        bool i = isDataseg() && !(storage_class & (STC.immutable_ | STC.const_ | STC.shared_ | STC.gshared));
        //printf("\treturn %d\n", i);
        return i;
    }

    /********************************************
     * Can variable be read and written by CTFE?
     */
    final bool isCTFE()
    {
        return (storage_class & STC.ctfe) != 0; // || !isDataseg();
    }

    final bool isOverlappedWith(VarDeclaration v)
    {
        const vsz = v.type.size();
        const tsz = type.size();
        assert(vsz != SIZE_INVALID && tsz != SIZE_INVALID);

        // Overlap is checked by comparing bit offsets
        auto bitoffset  =   offset * 8;
        auto vbitoffset = v.offset * 8;

        // Bitsize of types are overridden by any bit-field widths.
        ulong tbitsize = void;
        if (auto bf = isBitFieldDeclaration())
        {
            bitoffset += bf.bitOffset;
            tbitsize = bf.fieldWidth;
        }
        else
            tbitsize = tsz * 8;

        ulong vbitsize = void;
        if (auto vbf = v.isBitFieldDeclaration())
        {
            vbitoffset += vbf.bitOffset;
            vbitsize = vbf.fieldWidth;
        }
        else
            vbitsize = vsz * 8;

        return   bitoffset < vbitoffset + vbitsize &&
                vbitoffset <  bitoffset + tbitsize;
    }

    override final bool hasPointers()
    {
        //printf("VarDeclaration::hasPointers() %s, ty = %d\n", toChars(), type.ty);
        return (!isDataseg() && type.hasPointers());
    }

    /*************************************
     * Return true if we can take the address of this variable.
     */
    final bool canTakeAddressOf()
    {
        return !(storage_class & STC.manifest);
    }

    /******************************************
     * Return true if variable needs to call the destructor.
     */
    final bool needsScopeDtor()
    {
        //printf("VarDeclaration::needsScopeDtor() %s %d\n", toChars(), edtor && !(storage_class & STC.nodtor));
        return edtor && !(storage_class & STC.nodtor);
    }

    /******************************************
     * If a variable has a scope destructor call, return call for it.
     * Otherwise, return NULL.
     */
    extern (D) final Expression callScopeDtor(Scope* sc)
    {
        //printf("VarDeclaration::callScopeDtor() %s\n", toChars());

        // Destruction of STC.field's is handled by buildDtor()
        if (storage_class & (STC.nodtor | STC.ref_ | STC.out_ | STC.field))
        {
            return null;
        }

        if (iscatchvar)
            return null;    // destructor is built by `void semantic(Catch c, Scope* sc)`, not here

        Expression e = null;
        // Destructors for structs and arrays of structs
        Type tv = type.baseElemOf();
        if (tv.ty == Tstruct)
        {
            StructDeclaration sd = (cast(TypeStruct)tv).sym;
            if (!sd.dtor || sd.errors)
                return null;

            const sz = type.size();
            assert(sz != SIZE_INVALID);
            if (!sz)
                return null;

            if (type.toBasetype().ty == Tstruct)
            {
                // v.__xdtor()
                e = new VarExp(loc, this);

                /* This is a hack so we can call destructors on const/immutable objects.
                 * Need to add things like "const ~this()" and "immutable ~this()" to
                 * fix properly.
                 */
                e.type = e.type.mutableOf();

                // Enable calling destructors on shared objects.
                // The destructor is always a single, non-overloaded function,
                // and must serve both shared and non-shared objects.
                e.type = e.type.unSharedOf;

                e = new DotVarExp(loc, e, sd.dtor, false);
                e = new CallExp(loc, e);
            }
            else
            {
                // __ArrayDtor(v[0 .. n])
                e = new VarExp(loc, this);

                const sdsz = sd.type.size();
                assert(sdsz != SIZE_INVALID && sdsz != 0);
                const n = sz / sdsz;
                SliceExp se = new SliceExp(loc, e, new IntegerExp(loc, 0, Type.tsize_t),
                    new IntegerExp(loc, n, Type.tsize_t));

                // Prevent redundant bounds check
                se.upperIsInBounds = true;
                se.lowerIsLessThanUpper = true;

                // This is a hack so we can call destructors on const/immutable objects.
                se.type = sd.type.arrayOf();

                e = new CallExp(loc, new IdentifierExp(loc, Id.__ArrayDtor), se);
            }
            return e;
        }
        // Destructors for classes
        if (storage_class & (STC.auto_ | STC.scope_) && !(storage_class & STC.parameter))
        {
            for (ClassDeclaration cd = type.isClassHandle(); cd; cd = cd.baseClass)
            {
                /* We can do better if there's a way with onstack
                 * classes to determine if there's no way the monitor
                 * could be set.
                 */
                //if (cd.isInterfaceDeclaration())
                //    error("interface `%s` cannot be scope", cd.toChars());

                if (onstack) // if any destructors
                {
                    // delete'ing C++ classes crashes (and delete is deprecated anyway)
                    if (cd.classKind == ClassKind.cpp)
                    {
                        // Don't call non-existant dtor
                        if (!cd.dtor)
                            break;

                        e = new VarExp(loc, this);
                        e.type = e.type.mutableOf().unSharedOf(); // Hack for mutable ctor on immutable instances
                        e = new DotVarExp(loc, e, cd.dtor, false);
                        e = new CallExp(loc, e);
                        break;
                    }

                    // delete this;
                    Expression ec;
                    ec = new VarExp(loc, this);
                    e = new DeleteExp(loc, ec, true);
                    e.type = Type.tvoid;
                    break;
                }
            }
        }
        return e;
    }

    /*******************************************
     * If variable has a constant expression initializer, get it.
     * Otherwise, return null.
     */
    extern (D) final Expression getConstInitializer(bool needFullType = true)
    {
        assert(type && _init);

        // Ungag errors when not speculative
        uint oldgag = global.gag;
        if (global.gag)
        {
            Dsymbol sym = isMember();
            if (sym && !sym.isSpeculative())
                global.gag = 0;
        }

        if (_scope)
        {
            inuse++;
            _init = _init.initializerSemantic(_scope, type, INITinterpret);
            import dmd.semantic2 : lowerStaticAAs;
            lowerStaticAAs(this, _scope);
            _scope = null;
            inuse--;
        }

        Expression e = _init.initializerToExpression(needFullType ? type : null);
        global.gag = oldgag;
        return e;
    }

    /*******************************************
     * Helper function for the expansion of manifest constant.
     */
    extern (D) final Expression expandInitializer(Loc loc)
    {
        assert((storage_class & STC.manifest) && _init);

        auto e = getConstInitializer();
        if (!e)
        {
            .error(loc, "cannot make expression out of initializer for `%s`", toChars());
            return ErrorExp.get();
        }

        e = e.copy();
        e.loc = loc;    // for better error message
        return e;
    }

    override final void checkCtorConstInit()
    {
        version (none)
        {
            /* doesn't work if more than one static ctor */
            if (ctorinit == 0 && isCtorinit() && !isField())
                error("missing initializer in static constructor for const variable");
        }
    }

    /************************************
     * Check to see if this variable is actually in an enclosing function
     * rather than the current one.
     * Update nestedrefs[], closureVars[] and outerVars[].
     * Returns: true if error occurs.
     */
    extern (D) final bool checkNestedReference(Scope* sc, Loc loc)
    {
        //printf("VarDeclaration::checkNestedReference() %s\n", toChars());
        if (sc.intypeof == 1 || (sc.flags & SCOPE.ctfe))
            return false;
        if (!parent || parent == sc.parent)
            return false;
        if (isDataseg() || (storage_class & STC.manifest))
            return false;

        // The current function
        FuncDeclaration fdthis = sc.parent.isFuncDeclaration();
        if (!fdthis)
            return false; // out of function scope

        Dsymbol p = toParent2();

        // Function literals from fdthis to p must be delegates
        ensureStaticLinkTo(fdthis, p);

        // The function that this variable is in
        FuncDeclaration fdv = p.isFuncDeclaration();
        if (!fdv || fdv == fdthis)
            return false;

        // Add fdthis to nestedrefs[] if not already there
        if (!nestedrefs.contains(fdthis))
            nestedrefs.push(fdthis);

        //printf("\tfdv = %s\n", fdv.toChars());
        //printf("\tfdthis = %s\n", fdthis.toChars());
        if (loc.isValid())
        {
            if (fdthis.getLevelAndCheck(loc, sc, fdv, this) == fdthis.LevelError)
                return true;
        }

        // Add this VarDeclaration to fdv.closureVars[] if not already there
        if (!sc.intypeof && !(sc.flags & SCOPE.compile) &&
            // https://issues.dlang.org/show_bug.cgi?id=17605
            (fdv.skipCodegen || !fdthis.skipCodegen))
        {
            if (!fdv.closureVars.contains(this))
                fdv.closureVars.push(this);
        }

        if (!fdthis.outerVars.contains(this))
            fdthis.outerVars.push(this);

        //printf("fdthis is %s\n", fdthis.toChars());
        //printf("var %s in function %s is nested ref\n", toChars(), fdv.toChars());
        // __dollar creates problems because it isn't a real variable
        // https://issues.dlang.org/show_bug.cgi?id=3326
        if (ident == Id.dollar)
        {
            .error(loc, "cannnot use `$` inside a function literal");
            return true;
        }
        if (ident == Id.withSym) // https://issues.dlang.org/show_bug.cgi?id=1759
        {
            ExpInitializer ez = _init.isExpInitializer();
            assert(ez);
            Expression e = ez.exp;
            if (e.op == EXP.construct || e.op == EXP.blit)
                e = (cast(AssignExp)e).e2;
            return lambdaCheckForNestedRef(e, sc);
        }

        return false;
    }

    override final Dsymbol toAlias()
    {
        //printf("VarDeclaration::toAlias('%s', this = %p, aliassym = %p)\n", toChars(), this, aliassym);
        if ((!type || !type.deco) && _scope)
            dsymbolSemantic(this, _scope);

        assert(this != aliasTuple);
        Dsymbol s = aliasTuple ? aliasTuple.toAlias() : this;
        return s;
    }

    // Eliminate need for dynamic_cast
    override final inout(VarDeclaration) isVarDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/*******************************************************
 * C11 6.7.2.1-4 bit fields
 */
extern (C++) class BitFieldDeclaration : VarDeclaration
{
    Expression width;

    uint fieldWidth;
    uint bitOffset;

    final extern (D) this(const ref Loc loc, Type type, Identifier ident, Expression width)
    {
        super(loc, type, ident, null);

        this.width = width;
        this.storage_class |= STC.field;
    }

    override BitFieldDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("BitFieldDeclaration::syntaxCopy(%s)\n", toChars());
        assert(!s);
        auto bf = new BitFieldDeclaration(loc, type ? type.syntaxCopy() : null, ident, width.syntaxCopy());
        bf.comment = comment;
        return bf;
    }

    override final inout(BitFieldDeclaration) isBitFieldDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    /***********************************
     * Retrieve the .min or .max values.
     * Only valid after semantic analysis.
     * Params:
     *  id = Id.min or Id.max
     * Returns:
     *  the min or max value
     */
    final ulong getMinMax(Identifier id)
    {
        const width = fieldWidth;
        const uns = type.isunsigned();
        const min = id == Id.min;
        ulong v;
        assert(width != 0);  // should have been rejected in semantic pass
        if (width == ulong.sizeof * 8)
            v = uns ? (min ? ulong.min : ulong.max)
                    : (min ?  long.min :  long.max);
        else
            v = uns ? (min ? 0
                           : (1L << width) - 1)
                    : (min ? -(1L << (width - 1))
                           :  (1L << (width - 1)) - 1);
        return v;
    }
}

/***********************************************************
 * This is a shell around a back end symbol
 */
extern (C++) final class SymbolDeclaration : Declaration
{
    AggregateDeclaration dsym;

    extern (D) this(const ref Loc loc, AggregateDeclaration dsym) @safe
    {
        super(loc, dsym.ident);
        this.dsym = dsym;
        storage_class |= STC.const_;
    }

    // Eliminate need for dynamic_cast
    override inout(SymbolDeclaration) isSymbolDeclaration() inout
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
private Identifier getTypeInfoIdent(Type t)
{
    import dmd.dmangle;
    import core.stdc.stdlib;
    import dmd.root.rmem;
    // _init_10TypeInfo_%s
    OutBuffer buf;
    buf.reserve(32);
    mangleToBuffer(t, buf);

    const slice = buf[];

    // Allocate buffer on stack, fail over to using malloc()
    char[128] namebuf;
    const namelen = 19 + size_t.sizeof * 3 + slice.length + 1;
    auto name = namelen <= namebuf.length ? namebuf.ptr : cast(char*)Mem.check(malloc(namelen));

    const length = snprintf(name, namelen, "_D%lluTypeInfo_%.*s6__initZ",
            cast(ulong)(9 + slice.length), cast(int)slice.length, slice.ptr);
    //printf("%p %s, deco = %s, name = %s\n", this, toChars(), deco, name);
    assert(0 < length && length < namelen); // don't overflow the buffer

    auto id = Identifier.idPool(name[0 .. length]);

    if (name != namebuf.ptr)
        free(name);
    return id;
}

extern (C++) class TypeInfoDeclaration : VarDeclaration
{
    Type tinfo;

    final extern (D) this(Type tinfo)
    {
        super(Loc.initial, Type.dtypeinfo.type, tinfo.getTypeInfoIdent(), null);
        this.tinfo = tinfo;
        storage_class = STC.static_ | STC.gshared;
        visibility = Visibility(Visibility.Kind.public_);
        _linkage = LINK.c;
        alignment.set(target.ptrsize);
    }

    static TypeInfoDeclaration create(Type tinfo)
    {
        return new TypeInfoDeclaration(tinfo);
    }

    override final TypeInfoDeclaration syntaxCopy(Dsymbol s)
    {
        assert(0); // should never be produced by syntax
    }

    override final const(char)* toChars() const
    {
        //printf("TypeInfoDeclaration::toChars() tinfo = %s\n", tinfo.toChars());
        OutBuffer buf;
        buf.writestring("typeid(");
        buf.writestring(tinfo.toChars());
        buf.writeByte(')');
        return buf.extractChars();
    }

    override final inout(TypeInfoDeclaration) isTypeInfoDeclaration() inout @nogc nothrow pure @safe
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
extern (C++) final class TypeInfoStructDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfostruct)
        {
            ObjectNotFound(Id.TypeInfo_Struct);
        }
        type = Type.typeinfostruct.type;
    }

    static TypeInfoStructDeclaration create(Type tinfo)
    {
        return new TypeInfoStructDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoClassDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoclass)
        {
            ObjectNotFound(Id.TypeInfo_Class);
        }
        type = Type.typeinfoclass.type;
    }

    static TypeInfoClassDeclaration create(Type tinfo)
    {
        return new TypeInfoClassDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoInterfaceDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfointerface)
        {
            ObjectNotFound(Id.TypeInfo_Interface);
        }
        type = Type.typeinfointerface.type;
    }

    static TypeInfoInterfaceDeclaration create(Type tinfo)
    {
        return new TypeInfoInterfaceDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoPointerDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfopointer)
        {
            ObjectNotFound(Id.TypeInfo_Pointer);
        }
        type = Type.typeinfopointer.type;
    }

    static TypeInfoPointerDeclaration create(Type tinfo)
    {
        return new TypeInfoPointerDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoArrayDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoarray)
        {
            ObjectNotFound(Id.TypeInfo_Array);
        }
        type = Type.typeinfoarray.type;
    }

    static TypeInfoArrayDeclaration create(Type tinfo)
    {
        return new TypeInfoArrayDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoStaticArrayDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfostaticarray)
        {
            ObjectNotFound(Id.TypeInfo_StaticArray);
        }
        type = Type.typeinfostaticarray.type;
    }

    static TypeInfoStaticArrayDeclaration create(Type tinfo)
    {
        return new TypeInfoStaticArrayDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoAssociativeArrayDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoassociativearray)
        {
            ObjectNotFound(Id.TypeInfo_AssociativeArray);
        }
        type = Type.typeinfoassociativearray.type;
    }

    static TypeInfoAssociativeArrayDeclaration create(Type tinfo)
    {
        return new TypeInfoAssociativeArrayDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoEnumDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoenum)
        {
            ObjectNotFound(Id.TypeInfo_Enum);
        }
        type = Type.typeinfoenum.type;
    }

    static TypeInfoEnumDeclaration create(Type tinfo)
    {
        return new TypeInfoEnumDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoFunctionDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfofunction)
        {
            ObjectNotFound(Id.TypeInfo_Function);
        }
        type = Type.typeinfofunction.type;
    }

    static TypeInfoFunctionDeclaration create(Type tinfo)
    {
        return new TypeInfoFunctionDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoDelegateDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfodelegate)
        {
            ObjectNotFound(Id.TypeInfo_Delegate);
        }
        type = Type.typeinfodelegate.type;
    }

    static TypeInfoDelegateDeclaration create(Type tinfo)
    {
        return new TypeInfoDelegateDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoTupleDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfotypelist)
        {
            ObjectNotFound(Id.TypeInfo_Tuple);
        }
        type = Type.typeinfotypelist.type;
    }

    static TypeInfoTupleDeclaration create(Type tinfo)
    {
        return new TypeInfoTupleDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoConstDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoconst)
        {
            ObjectNotFound(Id.TypeInfo_Const);
        }
        type = Type.typeinfoconst.type;
    }

    static TypeInfoConstDeclaration create(Type tinfo)
    {
        return new TypeInfoConstDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoInvariantDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoinvariant)
        {
            ObjectNotFound(Id.TypeInfo_Invariant);
        }
        type = Type.typeinfoinvariant.type;
    }

    static TypeInfoInvariantDeclaration create(Type tinfo)
    {
        return new TypeInfoInvariantDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoSharedDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoshared)
        {
            ObjectNotFound(Id.TypeInfo_Shared);
        }
        type = Type.typeinfoshared.type;
    }

    static TypeInfoSharedDeclaration create(Type tinfo)
    {
        return new TypeInfoSharedDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoWildDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfowild)
        {
            ObjectNotFound(Id.TypeInfo_Wild);
        }
        type = Type.typeinfowild.type;
    }

    static TypeInfoWildDeclaration create(Type tinfo)
    {
        return new TypeInfoWildDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoVectorDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfovector)
        {
            ObjectNotFound(Id.TypeInfo_Vector);
        }
        type = Type.typeinfovector.type;
    }

    static TypeInfoVectorDeclaration create(Type tinfo)
    {
        return new TypeInfoVectorDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * For the "this" parameter to member functions
 */
extern (C++) final class ThisDeclaration : VarDeclaration
{
    extern (D) this(const ref Loc loc, Type t)
    {
        super(loc, t, Id.This, null);
        storage_class |= STC.nodtor;
    }

    override ThisDeclaration syntaxCopy(Dsymbol s)
    {
        assert(0); // should never be produced by syntax
    }

    override inout(ThisDeclaration) isThisDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
