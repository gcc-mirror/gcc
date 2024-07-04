/**
 * Template semantics.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/templatesem.d, _templatesem.d)
 * Documentation:  https://dlang.org/phobos/dmd_templatesem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/templatesem.d
 */

module dmd.templatesem;

import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.attrib;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.dinterpret;
import dmd.dmangle;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.impcnvtab;
import dmd.init;
import dmd.initsem;
import dmd.location;
import dmd.mtype;
import dmd.opover;
import dmd.optimize;
import dmd.root.array;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.semantic2;
import dmd.semantic3;
import dmd.templateparamsem;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

/************************************
 * Perform semantic analysis on template.
 * Params:
 *      sc = context
 *      tempdecl = template declaration
 */
void templateDeclarationSemantic(Scope* sc, TemplateDeclaration tempdecl)
{
    enum log = false;
    static if (log)
    {
        printf("TemplateDeclaration.dsymbolSemantic(this = %p, id = '%s')\n", this, tempdecl.ident.toChars());
        printf("sc.stc = %llx\n", sc.stc);
        printf("sc.module = %s\n", sc._module.toChars());
    }
    if (tempdecl.semanticRun != PASS.initial)
        return; // semantic() already run

    if (tempdecl._scope)
    {
        sc = tempdecl._scope;
        tempdecl._scope = null;
    }
    if (!sc)
        return;

    // Remember templates defined in module object that we need to know about
    if (sc._module && sc._module.ident == Id.object)
    {
        if (tempdecl.ident == Id.RTInfo)
            Type.rtinfo = tempdecl;
    }

    /* Remember Scope for later instantiations, but make
     * a copy since attributes can change.
     */
    if (!tempdecl._scope)
    {
        tempdecl._scope = sc.copy();
        tempdecl._scope.setNoFree();
    }

    tempdecl.semanticRun = PASS.semantic;

    tempdecl.parent = sc.parent;
    tempdecl.visibility = sc.visibility;
    tempdecl.userAttribDecl = sc.userAttribDecl;
    tempdecl.cppnamespace = sc.namespace;
    tempdecl.isstatic = tempdecl.toParent().isModule() || (tempdecl._scope.stc & STC.static_);
    tempdecl.deprecated_ = !!(sc.stc & STC.deprecated_);

    UserAttributeDeclaration.checkGNUABITag(tempdecl, sc.linkage);

    if (!tempdecl.isstatic)
    {
        if (auto ad = tempdecl.parent.pastMixin().isAggregateDeclaration())
            ad.makeNested();
    }

    // Set up scope for parameters
    auto paramsym = new ScopeDsymbol();
    paramsym.parent = tempdecl.parent;
    Scope* paramscope = sc.push(paramsym);
    paramscope.stc = 0;

    if (global.params.ddoc.doOutput)
    {
        tempdecl.origParameters = new TemplateParameters(tempdecl.parameters.length);
        for (size_t i = 0; i < tempdecl.parameters.length; i++)
        {
            TemplateParameter tp = (*tempdecl.parameters)[i];
            (*tempdecl.origParameters)[i] = tp.syntaxCopy();
        }
    }

    for (size_t i = 0; i < tempdecl.parameters.length; i++)
    {
        TemplateParameter tp = (*tempdecl.parameters)[i];
        if (!tp.declareParameter(paramscope))
        {
            error(tp.loc, "parameter `%s` multiply defined", tp.ident.toChars());
            tempdecl.errors = true;
        }
        if (!tp.tpsemantic(paramscope, tempdecl.parameters))
        {
            tempdecl.errors = true;
        }
        if (i + 1 != tempdecl.parameters.length && tp.isTemplateTupleParameter())
        {
            .error(tempdecl.loc, "%s `%s` template sequence parameter must be the last one", tempdecl.kind, tempdecl.toPrettyChars);
            tempdecl.errors = true;
        }
    }

    /* Calculate TemplateParameter.dependent
     */
    TemplateParameters tparams = TemplateParameters(1);
    for (size_t i = 0; i < tempdecl.parameters.length; i++)
    {
        TemplateParameter tp = (*tempdecl.parameters)[i];
        tparams[0] = tp;

        for (size_t j = 0; j < tempdecl.parameters.length; j++)
        {
            // Skip cases like: X(T : T)
            if (i == j)
                continue;

            if (TemplateTypeParameter ttp = (*tempdecl.parameters)[j].isTemplateTypeParameter())
            {
                if (reliesOnTident(ttp.specType, &tparams))
                    tp.dependent = true;
            }
            else if (TemplateAliasParameter tap = (*tempdecl.parameters)[j].isTemplateAliasParameter())
            {
                if (reliesOnTident(tap.specType, &tparams) ||
                    reliesOnTident(isType(tap.specAlias), &tparams))
                {
                    tp.dependent = true;
                }
            }
        }
    }

    paramscope.pop();

    // Compute again
    tempdecl.onemember = null;
    if (tempdecl.members)
    {
        Dsymbol s;
        if (Dsymbol.oneMembers(tempdecl.members, s, tempdecl.ident) && s)
        {
            tempdecl.onemember = s;
            s.parent = tempdecl;
        }
    }

    /* BUG: should check:
     *  1. template functions must not introduce virtual functions, as they
     *     cannot be accomodated in the vtbl[]
     *  2. templates cannot introduce non-static data members (i.e. fields)
     *     as they would change the instance size of the aggregate.
     */

    tempdecl.semanticRun = PASS.semanticdone;
}


/***************************************
 * Given that ti is an instance of this TemplateDeclaration,
 * deduce the types of the parameters to this, and store
 * those deduced types in dedtypes[].
 * Params:
 *  sc = context
 *  td = template
 *  ti = instance of td
 *  dedtypes = fill in with deduced types
 *  argumentList = arguments to template instance
 *  flag = 1 - don't do semantic() because of dummy types
 *         2 - don't change types in matchArg()
 * Returns: match level.
 */
public
MATCH matchWithInstance(Scope* sc, TemplateDeclaration td, TemplateInstance ti, ref Objects dedtypes, ArgumentList argumentList, int flag)
{
    enum LOGM = 0;
    static if (LOGM)
    {
        printf("\n+TemplateDeclaration.matchWithInstance(td = %s, ti = %s, flag = %d)\n", td.toChars(), ti.toChars(), flag);
    }
    version (none)
    {
        printf("dedtypes.length = %d, parameters.length = %d\n", dedtypes.length, parameters.length);
        if (ti.tiargs.length)
            printf("ti.tiargs.length = %d, [0] = %p\n", ti.tiargs.length, (*ti.tiargs)[0]);
    }
    MATCH nomatch()
    {
        static if (LOGM)
        {
            printf(" no match\n");
        }
        return MATCH.nomatch;
    }
    MATCH m;
    size_t dedtypes_dim = dedtypes.length;

    dedtypes.zero();

    if (td.errors)
        return MATCH.nomatch;

    size_t parameters_dim = td.parameters.length;
    const bool variadic = td.isVariadic() !is null;

    // If more arguments than parameters, no match
    if (ti.tiargs.length > parameters_dim && !variadic)
    {
        static if (LOGM)
        {
            printf(" no match: more arguments than parameters\n");
        }
        return MATCH.nomatch;
    }

    assert(dedtypes_dim == parameters_dim);
    assert(dedtypes_dim >= ti.tiargs.length || variadic);

    assert(td._scope);

    // Set up scope for template parameters
    Scope* paramscope = createScopeForTemplateParameters(td, ti, sc);

    // Attempt type deduction
    m = MATCH.exact;
    for (size_t i = 0; i < dedtypes_dim; i++)
    {
        MATCH m2;
        TemplateParameter tp = (*td.parameters)[i];
        Declaration sparam;

        //printf("\targument [%d]\n", i);
        static if (LOGM)
        {
            //printf("\targument [%d] is %s\n", i, oarg ? oarg.toChars() : "null");
            TemplateTypeParameter ttp = tp.isTemplateTypeParameter();
            if (ttp)
                printf("\tparameter[%d] is %s : %s\n", i, tp.ident.toChars(), ttp.specType ? ttp.specType.toChars() : "");
        }

        m2 = tp.matchArg(ti.loc, paramscope, ti.tiargs, i, td.parameters, dedtypes, &sparam);
        //printf("\tm2 = %d\n", m2);
        if (m2 == MATCH.nomatch)
        {
            version (none)
            {
                printf("\tmatchArg() for parameter %i failed\n", i);
            }
            return nomatch();
        }

        if (m2 < m)
            m = m2;

        if (!flag)
            sparam.dsymbolSemantic(paramscope);
        if (!paramscope.insert(sparam)) // TODO: This check can make more early
        {
            // in TemplateDeclaration.semantic, and
            // then we don't need to make sparam if flags == 0
            return nomatch();
        }
    }

    if (!flag)
    {
        /* Any parameter left without a type gets the type of
         * its corresponding arg
         */
        foreach (i, ref dedtype; dedtypes)
        {
            if (!dedtype)
            {
                assert(i < ti.tiargs.length);
                dedtype = cast(Type)(*ti.tiargs)[i];
            }
        }
    }

    if (m > MATCH.nomatch && td.constraint && !flag)
    {
        if (ti.hasNestedArgs(ti.tiargs, td.isstatic)) // TODO: should gag error
            ti.parent = ti.enclosing;
        else
            ti.parent = td.parent;

        // Similar to doHeaderInstantiation
        FuncDeclaration fd = td.onemember ? td.onemember.isFuncDeclaration() : null;
        if (fd)
        {
            TypeFunction tf = fd.type.isTypeFunction().syntaxCopy();

            fd = new FuncDeclaration(fd.loc, fd.endloc, fd.ident, fd.storage_class, tf);
            fd.parent = ti;
            fd.inferRetType = true;

            // Shouldn't run semantic on default arguments and return type.
            foreach (ref param; *tf.parameterList.parameters)
                param.defaultArg = null;

            tf.next = null;
            tf.incomplete = true;

            // Resolve parameter types and 'auto ref's.
            tf.inferenceArguments = argumentList;
            uint olderrors = global.startGagging();
            fd.type = tf.typeSemantic(td.loc, paramscope);
            global.endGagging(olderrors);
            if (fd.type.ty != Tfunction)
                return nomatch();
            fd.originalType = fd.type; // for mangling
        }

        // TODO: dedtypes => ti.tiargs ?
        if (!evaluateConstraint(td, ti, sc, paramscope, &dedtypes, fd))
            return nomatch();
    }

    static if (LOGM)
    {
        // Print out the results
        printf("--------------------------\n");
        printf("template %s\n", toChars());
        printf("instance %s\n", ti.toChars());
        if (m > MATCH.nomatch)
        {
            for (size_t i = 0; i < dedtypes_dim; i++)
            {
                TemplateParameter tp = (*parameters)[i];
                RootObject oarg;
                printf(" [%d]", i);
                if (i < ti.tiargs.length)
                    oarg = (*ti.tiargs)[i];
                else
                    oarg = null;
                tp.print(oarg, (*dedtypes)[i]);
            }
        }
        else
            return nomatch();
    }
    static if (LOGM)
    {
        printf(" match = %d\n", m);
    }

    paramscope.pop();
    static if (LOGM)
    {
        printf("-TemplateDeclaration.matchWithInstance(td = %s, ti = %s) = %d\n", td.toChars(), ti.toChars(), m);
    }
    return m;
}

/****************************
 * Check to see if constraint is satisfied.
 */
bool evaluateConstraint(TemplateDeclaration td, TemplateInstance ti, Scope* sc, Scope* paramscope, Objects* dedargs, FuncDeclaration fd)
{
    /* Detect recursive attempts to instantiate this template declaration,
     * https://issues.dlang.org/show_bug.cgi?id=4072
     *  void foo(T)(T x) if (is(typeof(foo(x)))) { }
     *  static assert(!is(typeof(foo(7))));
     * Recursive attempts are regarded as a constraint failure.
     */
    /* There's a chicken-and-egg problem here. We don't know yet if this template
     * instantiation will be a local one (enclosing is set), and we won't know until
     * after selecting the correct template. Thus, function we're nesting inside
     * is not on the sc scope chain, and this can cause errors in FuncDeclaration.getLevel().
     * Workaround the problem by setting a flag to relax the checking on frame errors.
     */

    for (TemplatePrevious* p = td.previous; p; p = p.prev)
    {
        if (!arrayObjectMatch(*p.dedargs, *dedargs))
            continue;
        //printf("recursive, no match p.sc=%p %p %s\n", p.sc, this, this.toChars());
        /* It must be a subscope of p.sc, other scope chains are not recursive
         * instantiations.
         * the chain of enclosing scopes is broken by paramscope (its enclosing
         * scope is _scope, but paramscope.callsc is the instantiating scope). So
         * it's good enough to check the chain of callsc
         */
        for (Scope* scx = paramscope.callsc; scx; scx = scx.callsc)
        {
            // The first scx might be identical for nested eponymeous templates, e.g.
            // template foo() { void foo()() {...} }
            if (scx == p.sc && scx !is paramscope.callsc)
                return false;
        }
        /* BUG: should also check for ref param differences
         */
    }

    TemplatePrevious pr;
    pr.prev = td.previous;
    pr.sc = paramscope.callsc;
    pr.dedargs = dedargs;
    td.previous = &pr; // add this to threaded list

    Scope* scx = paramscope.push(ti);
    scx.parent = ti;
    scx.tinst = null;
    scx.minst = null;
    // Set SCOPE.constraint before declaring function parameters for the static condition
    // (previously, this was immediately before calling evalStaticCondition), so the
    // semantic pass knows not to issue deprecation warnings for these throw-away decls.
    // https://issues.dlang.org/show_bug.cgi?id=21831
    scx.flags |= SCOPE.constraint;

    assert(!ti.symtab);
    if (fd)
    {
        /* Declare all the function parameters as variables and add them to the scope
         * Making parameters is similar to FuncDeclaration.semantic3
         */
        auto tf = fd.type.isTypeFunction();

        scx.parent = fd;

        Parameters* fparameters = tf.parameterList.parameters;
        const nfparams = tf.parameterList.length;
        foreach (i, fparam; tf.parameterList)
        {
            fparam.storageClass &= (STC.IOR | STC.lazy_ | STC.final_ | STC.TYPECTOR | STC.nodtor);
            fparam.storageClass |= STC.parameter;
            if (tf.parameterList.varargs == VarArg.typesafe && i + 1 == nfparams)
            {
                fparam.storageClass |= STC.variadic;
                /* Don't need to set STC.scope_ because this will only
                 * be evaluated at compile time
                 */
            }
        }
        foreach (fparam; *fparameters)
        {
            if (!fparam.ident)
                continue;
            // don't add it, if it has no name
            auto v = new VarDeclaration(fparam.loc, fparam.type, fparam.ident, null);
            fparam.storageClass |= STC.parameter;
            v.storage_class = fparam.storageClass;
            v.dsymbolSemantic(scx);
            if (!ti.symtab)
                ti.symtab = new DsymbolTable();
            if (!scx.insert(v))
                .error(td.loc, "%s `%s` parameter `%s.%s` is already defined", td.kind, td.toPrettyChars, td.toChars(), v.toChars());
            else
                v.parent = fd;
        }
        if (td.isstatic)
            fd.storage_class |= STC.static_;
        declareThis(fd, scx);
    }

    td.lastConstraint = td.constraint.syntaxCopy();
    td.lastConstraintTiargs = ti.tiargs;
    td.lastConstraintNegs.setDim(0);

    import dmd.staticcond;

    assert(ti.inst is null);
    ti.inst = ti; // temporary instantiation to enable genIdent()
    bool errors;
    const bool result = evalStaticCondition(scx, td.constraint, td.lastConstraint, errors, &td.lastConstraintNegs);
    if (result || errors)
    {
        td.lastConstraint = null;
        td.lastConstraintTiargs = null;
        td.lastConstraintNegs.setDim(0);
    }
    ti.inst = null;
    ti.symtab = null;
    scx = scx.pop();
    td.previous = pr.prev; // unlink from threaded list
    if (errors)
        return false;
    return result;
}

/*******************************************
 * Append to buf a textual representation of template parameters with their arguments.
 * Params:
 *  parameters = the template parameters
 *  tiargs = the correspondeing template arguments
 *  variadic = if it's a variadic argument list
 *  buf = where the text output goes
 */
void formatParamsWithTiargs(ref TemplateParameters parameters, ref Objects tiargs, bool variadic, ref OutBuffer buf)
{
    buf.writestring("  with `");

    // write usual arguments line-by-line
    // skips trailing default ones - they are not present in `tiargs`
    const end = parameters.length - (variadic ? 1 : 0);
    size_t i;
    for (; i < tiargs.length && i < end; i++)
    {
        if (i)
        {
            buf.writeByte(',');
            buf.writenl();
            buf.writestring("       ");
        }
        write(buf, parameters[i]);
        buf.writestring(" = ");
        write(buf, tiargs[i]);
    }
    // write remaining variadic arguments on the last line
    if (variadic)
    {
        if (i)
        {
            buf.writeByte(',');
            buf.writenl();
            buf.writestring("       ");
        }
        write(buf, parameters[end]);
        buf.writestring(" = ");
        buf.writeByte('(');
        if (end < tiargs.length)
        {
            write(buf, tiargs[end]);
            foreach (j; parameters.length .. tiargs.length)
            {
                buf.writestring(", ");
                write(buf, tiargs[j]);
            }
        }
        buf.writeByte(')');
    }
    buf.writeByte('`');
}

/******************************
 * Create a scope for the parameters of the TemplateInstance
 * `ti` in the parent scope sc from the ScopeDsymbol paramsym.
 *
 * If paramsym is null a new ScopeDsymbol is used in place of
 * paramsym.
 * Params:
 *      td = template that ti is an instance of
 *      ti = the TemplateInstance whose parameters to generate the scope for.
 *      sc = the parent scope of ti
 * Returns:
 *      new scope for the parameters of ti
 */
Scope* createScopeForTemplateParameters(TemplateDeclaration td, TemplateInstance ti, Scope* sc)
{
    ScopeDsymbol paramsym = new ScopeDsymbol();
    paramsym.parent = td._scope.parent;
    Scope* paramscope = td._scope.push(paramsym);
    paramscope.tinst = ti;
    paramscope.minst = sc.minst;
    paramscope.callsc = sc;
    paramscope.stc = 0;
    return paramscope;
}

/********************************************
 * Determine partial specialization order of `td` vs `td2`.
 * Params:
 *  sc = context
 *  td = first template
 *  td2 = second template
 *  argumentList = arguments to template
 * Returns:
 *      MATCH - td is at least as specialized as td2
 *      MATCH.nomatch - td2 is more specialized than td
 */
MATCH leastAsSpecialized(Scope* sc, TemplateDeclaration td, TemplateDeclaration td2, ArgumentList argumentList)
{
    enum LOG_LEASTAS = 0;
    static if (LOG_LEASTAS)
    {
        printf("%s.leastAsSpecialized(%s)\n", toChars(), td2.toChars());
    }

    /* This works by taking the template parameters to this template
     * declaration and feeding them to td2 as if it were a template
     * instance.
     * If it works, then this template is at least as specialized
     * as td2.
     */

    // Set type arguments to dummy template instance to be types
    // generated from the parameters to this template declaration
    auto tiargs = new Objects();
    tiargs.reserve(td.parameters.length);
    foreach (tp; *td.parameters)
    {
        if (tp.dependent)
            break;
        RootObject p = tp.dummyArg();
        if (!p) //TemplateTupleParameter
            break;

        tiargs.push(p);
    }
    scope TemplateInstance ti = new TemplateInstance(Loc.initial, td.ident, tiargs); // create dummy template instance

    // Temporary Array to hold deduced types
    Objects dedtypes = Objects(td2.parameters.length);

    // Attempt a type deduction
    MATCH m = matchWithInstance(sc, td2, ti, dedtypes, argumentList, 1);
    if (m > MATCH.nomatch)
    {
        /* A non-variadic template is more specialized than a
         * variadic one.
         */
        TemplateTupleParameter tp = td.isVariadic();
        if (tp && !tp.dependent && !td2.isVariadic())
            goto L1;

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

/*************************************************
 * Match function arguments against a specific template function.
 *
 * Params:
 *     td = template declaration for template instance
 *     ti = template instance. `ti.tdtypes` will be set to Expression/Type deduced template arguments
 *     sc = instantiation scope
 *     fd = Partially instantiated function declaration, which is set to an instantiated function declaration
 *     tthis = 'this' argument if !NULL
 *     argumentList = arguments to function
 *
 * Returns:
 *      match pair of initial and inferred template arguments
 */
extern (D) MATCHpair deduceFunctionTemplateMatch(TemplateDeclaration td, TemplateInstance ti, Scope* sc, ref FuncDeclaration fd, Type tthis, ArgumentList argumentList)
{
    version (none)
    {
        printf("\nTemplateDeclaration.deduceFunctionTemplateMatch() %s\n", td.toChars());
        for (size_t i = 0; i < (fargs ? fargs.length : 0); i++)
        {
            Expression e = (*fargs)[i];
            printf("\tfarg[%d] is %s, type is %s\n", cast(int) i, e.toChars(), e.type.toChars());
        }
        printf("fd = %s\n", fd.toChars());
        printf("fd.type = %s\n", fd.type.toChars());
        if (tthis)
            printf("tthis = %s\n", tthis.toChars());
    }

    assert(td._scope);

    auto dedargs = new Objects(td.parameters.length);
    dedargs.zero();

    Objects* dedtypes = &ti.tdtypes; // for T:T*, the dedargs is the T*, dedtypes is the T
    dedtypes.setDim(td.parameters.length);
    dedtypes.zero();

    if (td.errors || fd.errors)
        return MATCHpair(MATCH.nomatch, MATCH.nomatch);

    // Set up scope for parameters
    Scope* paramscope = createScopeForTemplateParameters(td, ti,sc);

    MATCHpair nomatch()
    {
        paramscope.pop();
        //printf("\tnomatch\n");
        return MATCHpair(MATCH.nomatch, MATCH.nomatch);
    }

    MATCHpair matcherror()
    {
        // todo: for the future improvement
        paramscope.pop();
        //printf("\terror\n");
        return MATCHpair(MATCH.nomatch, MATCH.nomatch);
    }
    // Mark the parameter scope as deprecated if the templated
    // function is deprecated (since paramscope.enclosing is the
    // calling scope already)
    paramscope.stc |= fd.storage_class & STC.deprecated_;

    TemplateTupleParameter tp = td.isVariadic();
    Tuple declaredTuple = null;

    version (none)
    {
        for (size_t i = 0; i < dedargs.length; i++)
        {
            printf("\tdedarg[%d] = ", i);
            RootObject oarg = (*dedargs)[i];
            if (oarg)
                printf("%s", oarg.toChars());
            printf("\n");
        }
    }

    size_t ntargs = 0; // array size of tiargs
    size_t inferStart = 0; // index of first template parameter to infer from function argument
    const Loc instLoc = ti.loc;
    MATCH matchTiargs = MATCH.exact;

    if (auto tiargs = ti.tiargs)
    {
        // Set initial template arguments
        ntargs = tiargs.length;
        size_t n = td.parameters.length;
        if (tp)
            n--;
        if (ntargs > n)
        {
            if (!tp)
                return nomatch();

            /* The extra initial template arguments
             * now form the tuple argument.
             */
            auto t = new Tuple(ntargs - n);
            assert(td.parameters.length);
            (*dedargs)[td.parameters.length - 1] = t;

            for (size_t i = 0; i < t.objects.length; i++)
            {
                t.objects[i] = (*tiargs)[n + i];
            }
            td.declareParameter(paramscope, tp, t);
            declaredTuple = t;
        }
        else
            n = ntargs;

        memcpy(dedargs.tdata(), tiargs.tdata(), n * (*dedargs.tdata()).sizeof);

        for (size_t i = 0; i < n; i++)
        {
            assert(i < td.parameters.length);
            Declaration sparam = null;
            MATCH m = (*td.parameters)[i].matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, &sparam);
            //printf("\tdeduceType m = %d\n", m);
            if (m == MATCH.nomatch)
                return nomatch();
            if (m < matchTiargs)
                matchTiargs = m;

            sparam.dsymbolSemantic(paramscope);
            if (!paramscope.insert(sparam))
                return nomatch();
        }
        if (n < td.parameters.length && !declaredTuple)
        {
            inferStart = n;
        }
        else
            inferStart = td.parameters.length;
        //printf("tiargs matchTiargs = %d\n", matchTiargs);
    }
    version (none)
    {
        for (size_t i = 0; i < dedargs.length; i++)
        {
            printf("\tdedarg[%d] = ", i);
            RootObject oarg = (*dedargs)[i];
            if (oarg)
                printf("%s", oarg.toChars());
            printf("\n");
        }
    }

    ParameterList fparameters = fd.getParameterList(); // function parameter list
    const nfparams = fparameters.length; // number of function parameters

    /* Check for match of function arguments with variadic template
     * parameter, such as:
     *
     * void foo(T, A...)(T t, A a);
     * void main() { foo(1,2,3); }
     */
    size_t fptupindex = IDX_NOTFOUND;
    if (tp) // if variadic
    {
        // TemplateTupleParameter always makes most lesser matching.
        matchTiargs = MATCH.convert;

        if (nfparams == 0 && argumentList.length != 0) // if no function parameters
        {
            if (!declaredTuple)
            {
                auto t = new Tuple();
                //printf("t = %p\n", t);
                (*dedargs)[td.parameters.length - 1] = t;
                td.declareParameter(paramscope, tp, t);
                declaredTuple = t;
            }
        }
        else
        {
            /* Figure out which of the function parameters matches
             * the tuple template parameter. Do this by matching
             * type identifiers.
             * Set the index of this function parameter to fptupindex.
             */
            for (fptupindex = 0; fptupindex < nfparams; fptupindex++)
            {
                auto fparam = (*fparameters.parameters)[fptupindex]; // fparameters[fptupindex] ?
                if (fparam.type.ty != Tident)
                    continue;
                TypeIdentifier tid = fparam.type.isTypeIdentifier();
                if (!tp.ident.equals(tid.ident) || tid.idents.length)
                    continue;

                if (fparameters.varargs != VarArg.none) // variadic function doesn't
                    return nomatch(); // go with variadic template

                goto L1;
            }
            fptupindex = IDX_NOTFOUND;
        L1:
        }
    }

    MATCH match = MATCH.exact;
    if (td.toParent().isModule())
        tthis = null;
    if (tthis)
    {
        bool hasttp = false;

        // Match 'tthis' to any TemplateThisParameter's
        foreach (param; *td.parameters)
        {
            if (auto ttp = param.isTemplateThisParameter())
            {
                hasttp = true;

                Type t = new TypeIdentifier(Loc.initial, ttp.ident);
                MATCH m = deduceType(tthis, paramscope, t, *td.parameters, *dedtypes);
                if (m == MATCH.nomatch)
                    return nomatch();
                if (m < match)
                    match = m; // pick worst match
            }
        }

        // Match attributes of tthis against attributes of fd
        if (fd.type && !fd.isCtorDeclaration() && !(td._scope.stc & STC.static_))
        {
            StorageClass stc = td._scope.stc | fd.storage_class2;
            // Propagate parent storage class, https://issues.dlang.org/show_bug.cgi?id=5504
            Dsymbol p = td.parent;
            while (p.isTemplateDeclaration() || p.isTemplateInstance())
                p = p.parent;
            AggregateDeclaration ad = p.isAggregateDeclaration();
            if (ad)
                stc |= ad.storage_class;

            ubyte mod = fd.type.mod;
            if (stc & STC.immutable_)
                mod = MODFlags.immutable_;
            else
            {
                if (stc & (STC.shared_ | STC.synchronized_))
                    mod |= MODFlags.shared_;
                if (stc & STC.const_)
                    mod |= MODFlags.const_;
                if (stc & STC.wild)
                    mod |= MODFlags.wild;
            }

            ubyte thismod = tthis.mod;
            if (hasttp)
                mod = MODmerge(thismod, mod);
            MATCH m = MODmethodConv(thismod, mod);
            if (m == MATCH.nomatch)
                return nomatch();
            if (m < match)
                match = m;
        }
    }

    // Loop through the function parameters
    {
        //printf("%s\n\tnfargs = %d, nfparams = %d, tuple_dim = %d\n", toChars(), nfargs, nfparams, declaredTuple ? declaredTuple.objects.length : 0);
        //printf("\ttp = %p, fptupindex = %d, found = %d, declaredTuple = %s\n", tp, fptupindex, fptupindex != IDX_NOTFOUND, declaredTuple ? declaredTuple.toChars() : NULL);
        enum DEFAULT_ARGI = size_t.max - 10; // pseudo index signifying the parameter is expected to be assigned its default argument
        size_t argi = 0; // current argument index
        size_t argsConsumed = 0; // to ensure no excess arguments
        size_t nfargs2 = argumentList.length; // total number of arguments including applied defaultArgs
        uint inoutMatch = 0; // for debugging only
        Expression[] fargs = argumentList.arguments ? (*argumentList.arguments)[] : null;
        Identifier[] fnames = argumentList.names ? (*argumentList.names)[] : null;

        for (size_t parami = 0; parami < nfparams; parami++)
        {
            Parameter fparam = fparameters[parami];

            // Apply function parameter storage classes to parameter types
            Type prmtype = fparam.type.addStorageClass(fparam.storageClass);

            Expression farg;
            Identifier fname = argi < fnames.length ? fnames[argi] : null;
            bool foundName = false;
            if (fparam.ident)
            {
                foreach (i; 0 .. fnames.length)
                {
                    if (fparam.ident == fnames[i])
                    {
                        argi = i;
                        foundName = true;
                    }
                }
            }
            if (fname && !foundName)
            {
                argi = DEFAULT_ARGI;
            }

            /* See function parameters which wound up
             * as part of a template tuple parameter.
             */
            if (fptupindex != IDX_NOTFOUND && parami == fptupindex && argi != DEFAULT_ARGI)
            {
                TypeIdentifier tid = prmtype.isTypeIdentifier();
                assert(tid);
                if (!declaredTuple)
                {
                    /* The types of the function arguments
                     * now form the tuple argument.
                     */
                    declaredTuple = new Tuple();
                    (*dedargs)[td.parameters.length - 1] = declaredTuple;

                    /* Count function parameters with no defaults following a tuple parameter.
                     * void foo(U, T...)(int y, T, U, double, int bar = 0) {}  // rem == 2 (U, double)
                     */
                    size_t rem = 0;
                    foreach (j; parami + 1 .. nfparams)
                    {
                        Parameter p = fparameters[j];
                        if (p.defaultArg)
                        {
                            break;
                        }
                        foreach(name; fnames)
                        {
                            if (p.ident == name)
                                break;
                        }
                        if (!reliesOnTemplateParameters(p.type, (*td.parameters)[inferStart .. td.parameters.length]))
                        {
                            Type pt = p.type.syntaxCopy().typeSemantic(fd.loc, paramscope);
                            if (auto ptt = pt.isTypeTuple())
                                rem += ptt.arguments.length;
                            else
                                rem += 1;
                        }
                        else
                        {
                            ++rem;
                        }
                    }

                    if (nfargs2 - argi < rem)
                        return nomatch();
                    declaredTuple.objects.setDim(nfargs2 - argi - rem);
                    foreach (i; 0 .. declaredTuple.objects.length)
                    {
                        farg = fargs[argi + i];

                        // Check invalid arguments to detect errors early.
                        if (farg.op == EXP.error || farg.type.ty == Terror)
                            return nomatch();

                        if (!fparam.isLazy() && farg.type.ty == Tvoid)
                            return nomatch();

                        Type tt;
                        MATCH m;
                        if (ubyte wm = deduceWildHelper(farg.type, &tt, tid))
                        {
                            inoutMatch |= wm;
                            m = MATCH.constant;
                        }
                        else
                        {
                            m = deduceTypeHelper(farg.type, tt, tid);
                        }
                        if (m == MATCH.nomatch)
                            return nomatch();
                        if (m < match)
                            match = m;

                        /* Remove top const for dynamic array types and pointer types
                         */
                        if ((tt.ty == Tarray || tt.ty == Tpointer) && !tt.isMutable() && (!(fparam.storageClass & STC.ref_) || (fparam.storageClass & STC.auto_) && !farg.isLvalue()))
                        {
                            tt = tt.mutableOf();
                        }
                        declaredTuple.objects[i] = tt;
                    }
                    td.declareParameter(paramscope, tp, declaredTuple);
                }
                else
                {
                    // https://issues.dlang.org/show_bug.cgi?id=6810
                    // If declared tuple is not a type tuple,
                    // it cannot be function parameter types.
                    for (size_t i = 0; i < declaredTuple.objects.length; i++)
                    {
                        if (!isType(declaredTuple.objects[i]))
                            return nomatch();
                    }
                }
                assert(declaredTuple);
                argi += declaredTuple.objects.length;
                argsConsumed += declaredTuple.objects.length;
                continue;
            }

            // If parameter type doesn't depend on inferred template parameters,
            // semantic it to get actual type.
            if (!reliesOnTemplateParameters(prmtype, (*td.parameters)[inferStart .. td.parameters.length]))
            {
                // should copy prmtype to avoid affecting semantic result
                prmtype = prmtype.syntaxCopy().typeSemantic(fd.loc, paramscope);

                if (TypeTuple tt = prmtype.isTypeTuple())
                {
                    const tt_dim = tt.arguments.length;
                    for (size_t j = 0; j < tt_dim; j++, ++argi, ++argsConsumed)
                    {
                        Parameter p = (*tt.arguments)[j];
                        if (j == tt_dim - 1 && fparameters.varargs == VarArg.typesafe &&
                            parami + 1 == nfparams && argi < fargs.length)
                        {
                            prmtype = p.type;
                            goto Lvarargs;
                        }
                        if (argi >= fargs.length)
                        {
                            if (p.defaultArg)
                                continue;

                            // https://issues.dlang.org/show_bug.cgi?id=19888
                            if (fparam.defaultArg)
                                break;

                            return nomatch();
                        }
                        farg = fargs[argi];
                        if (!farg.implicitConvTo(p.type))
                            return nomatch();
                    }
                    continue;
                }
            }

            if (argi >= fargs.length) // if not enough arguments
            {
                if (!fparam.defaultArg)
                    goto Lvarargs;

                /* https://issues.dlang.org/show_bug.cgi?id=2803
                 * Before the starting of type deduction from the function
                 * default arguments, set the already deduced parameters into paramscope.
                 * It's necessary to avoid breaking existing acceptable code. Cases:
                 *
                 * 1. Already deduced template parameters can appear in fparam.defaultArg:
                 *  auto foo(A, B)(A a, B b = A.stringof);
                 *  foo(1);
                 *  // at fparam == 'B b = A.string', A is equivalent with the deduced type 'int'
                 *
                 * 2. If prmtype depends on default-specified template parameter, the
                 * default type should be preferred.
                 *  auto foo(N = size_t, R)(R r, N start = 0)
                 *  foo([1,2,3]);
                 *  // at fparam `N start = 0`, N should be 'size_t' before
                 *  // the deduction result from fparam.defaultArg.
                 */
                if (argi == fargs.length)
                {
                    foreach (ref dedtype; *dedtypes)
                    {
                        Type at = isType(dedtype);
                        if (at && at.ty == Tnone)
                        {
                            TypeDeduced xt = cast(TypeDeduced)at;
                            dedtype = xt.tded; // 'unbox'
                        }
                    }
                    for (size_t i = ntargs; i < dedargs.length; i++)
                    {
                        TemplateParameter tparam = (*td.parameters)[i];

                        RootObject oarg = (*dedargs)[i];
                        RootObject oded = (*dedtypes)[i];
                        if (oarg)
                            continue;

                        if (oded)
                        {
                            if (tparam.specialization() || !tparam.isTemplateTypeParameter())
                            {
                                /* The specialization can work as long as afterwards
                                 * the oded == oarg
                                 */
                                (*dedargs)[i] = oded;
                                MATCH m2 = tparam.matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, null);
                                //printf("m2 = %d\n", m2);
                                if (m2 == MATCH.nomatch)
                                    return nomatch();
                                if (m2 < matchTiargs)
                                    matchTiargs = m2; // pick worst match
                                if (!(*dedtypes)[i].equals(oded))
                                    .error(td.loc, "%s `%s` specialization not allowed for deduced parameter `%s`",
                                        td.kind, td.toPrettyChars, td.kind, td.toPrettyChars, tparam.ident.toChars());
                            }
                            else
                            {
                                if (MATCH.convert < matchTiargs)
                                    matchTiargs = MATCH.convert;
                            }
                            (*dedargs)[i] = td.declareParameter(paramscope, tparam, oded);
                        }
                        else
                        {
                            oded = tparam.defaultArg(instLoc, paramscope);
                            if (oded)
                                (*dedargs)[i] = td.declareParameter(paramscope, tparam, oded);
                        }
                    }
                }

                if (argi != DEFAULT_ARGI)
                    nfargs2 = argi + 1;

                /* If prmtype does not depend on any template parameters:
                 *
                 *  auto foo(T)(T v, double x = 0);
                 *  foo("str");
                 *  // at fparam == 'double x = 0'
                 *
                 * or, if all template parameters in the prmtype are already deduced:
                 *
                 *  auto foo(R)(R range, ElementType!R sum = 0);
                 *  foo([1,2,3]);
                 *  // at fparam == 'ElementType!R sum = 0'
                 *
                 * Deducing prmtype from fparam.defaultArg is not necessary.
                 */
                if (prmtype.deco || prmtype.syntaxCopy().trySemantic(td.loc, paramscope))
                {
                    if (argi != DEFAULT_ARGI)
                    {
                        ++argi;
                        ++argsConsumed;
                    }
                    continue;
                }

                // Deduce prmtype from the defaultArg.
                farg = fparam.defaultArg.syntaxCopy();
                farg = farg.expressionSemantic(paramscope);
                farg = resolveProperties(paramscope, farg);
            }
            else
            {
                farg = fargs[argi];
            }
            {
                assert(farg);
                // Check invalid arguments to detect errors early.
                if (farg.op == EXP.error || farg.type.ty == Terror)
                    return nomatch();

                Type att = null;
            Lretry:
                version (none)
                {
                    printf("\tfarg.type   = %s\n", farg.type.toChars());
                    printf("\tfparam.type = %s\n", prmtype.toChars());
                }
                Type argtype = farg.type;

                if (!fparam.isLazy() && argtype.ty == Tvoid && farg.op != EXP.function_)
                    return nomatch();

                // https://issues.dlang.org/show_bug.cgi?id=12876
                // Optimize argument to allow CT-known length matching
                farg = farg.optimize(WANTvalue, fparam.isReference());
                //printf("farg = %s %s\n", farg.type.toChars(), farg.toChars());

                RootObject oarg = farg;
                if ((fparam.storageClass & STC.ref_) && (!(fparam.storageClass & STC.auto_) || farg.isLvalue()))
                {
                    /* Allow expressions that have CT-known boundaries and type [] to match with [dim]
                     */
                    bool inferIndexType = (argtype.ty == Tarray) && (prmtype.ty == Tsarray || prmtype.ty == Taarray);
                    if (auto aaType = prmtype.isTypeAArray())
                    {
                        if (auto indexType = aaType.index.isTypeIdentifier())
                        {
                            inferIndexType = indexType.idents.length == 0;
                        }
                    }
                    if (inferIndexType)
                    {
                        if (StringExp se = farg.isStringExp())
                        {
                            argtype = se.type.nextOf().sarrayOf(se.len);
                        }
                        else if (ArrayLiteralExp ae = farg.isArrayLiteralExp())
                        {
                            argtype = ae.type.nextOf().sarrayOf(ae.elements.length);
                        }
                        else if (SliceExp se = farg.isSliceExp())
                        {
                            if (Type tsa = toStaticArrayType(se))
                                argtype = tsa;
                        }
                    }

                    oarg = argtype;
                }
                else if ((fparam.storageClass & STC.out_) == 0 &&
                         (argtype.ty == Tarray || argtype.ty == Tpointer) &&
                         templateParameterLookup(prmtype, td.parameters) != IDX_NOTFOUND &&
                         prmtype.isTypeIdentifier().idents.length == 0)
                {
                    /* The farg passing to the prmtype always make a copy. Therefore,
                     * we can shrink the set of the deduced type arguments for prmtype
                     * by adjusting top-qualifier of the argtype.
                     *
                     *  prmtype         argtype     ta
                     *  T            <- const(E)[]  const(E)[]
                     *  T            <- const(E[])  const(E)[]
                     *  qualifier(T) <- const(E)[]  const(E[])
                     *  qualifier(T) <- const(E[])  const(E[])
                     */
                    Type ta = argtype.castMod(prmtype.mod ? argtype.nextOf().mod : 0);
                    if (ta != argtype)
                    {
                        Expression ea = farg.copy();
                        ea.type = ta;
                        oarg = ea;
                    }
                }

                if (fparameters.varargs == VarArg.typesafe && parami + 1 == nfparams && argi + 1 < fargs.length)
                    goto Lvarargs;

                uint im = 0;
                MATCH m = deduceType(oarg, paramscope, prmtype, *td.parameters, *dedtypes, &im, inferStart);
                //printf("\tL%d deduceType m = %d, im = x%x, inoutMatch = x%x\n", __LINE__, m, im, inoutMatch);
                inoutMatch |= im;

                /* If no match, see if the argument can be matched by using
                 * implicit conversions.
                 */
                if (m == MATCH.nomatch && prmtype.deco)
                    m = farg.implicitConvTo(prmtype);

                if (m == MATCH.nomatch)
                {
                    AggregateDeclaration ad = isAggregate(farg.type);
                    if (ad && ad.aliasthis && !isRecursiveAliasThis(att, argtype))
                    {
                        // https://issues.dlang.org/show_bug.cgi?id=12537
                        // The isRecursiveAliasThis() call above

                        /* If a semantic error occurs while doing alias this,
                         * eg purity(https://issues.dlang.org/show_bug.cgi?id=7295),
                         * just regard it as not a match.
                         *
                         * We also save/restore sc.func.flags to avoid messing up
                         * attribute inference in the evaluation.
                        */
                        const oldflags = sc.func ? sc.func.flags : 0;
                        auto e = resolveAliasThis(sc, farg, true);
                        if (sc.func)
                            sc.func.flags = oldflags;
                        if (e)
                        {
                            farg = e;
                            goto Lretry;
                        }
                    }
                }

                if (m > MATCH.nomatch && (fparam.storageClass & (STC.ref_ | STC.auto_)) == STC.ref_)
                {
                    if (!farg.isLvalue())
                    {
                        if ((farg.op == EXP.string_ || farg.op == EXP.slice) && (prmtype.ty == Tsarray || prmtype.ty == Taarray))
                        {
                            // Allow conversion from T[lwr .. upr] to ref T[upr-lwr]
                        }
                        else if (global.params.rvalueRefParam == FeatureState.enabled)
                        {
                            // Allow implicit conversion to ref
                        }
                        else
                            return nomatch();
                    }
                }
                if (m > MATCH.nomatch && (fparam.storageClass & STC.out_))
                {
                    if (!farg.isLvalue())
                        return nomatch();
                    if (!farg.type.isMutable()) // https://issues.dlang.org/show_bug.cgi?id=11916
                        return nomatch();
                }
                if (m == MATCH.nomatch && fparam.isLazy() && prmtype.ty == Tvoid && farg.type.ty != Tvoid)
                    m = MATCH.convert;
                if (m != MATCH.nomatch)
                {
                    if (m < match)
                        match = m; // pick worst match
                    if (argi != DEFAULT_ARGI)
                    {
                        argi++;
                        argsConsumed++;
                    }
                    continue;
                }
            }

        Lvarargs:
            /* The following code for variadic arguments closely
             * matches TypeFunction.callMatch()
             */
            if (!(fparameters.varargs == VarArg.typesafe && parami + 1 == nfparams))
                return nomatch();

            /* Check for match with function parameter T...
             */
            Type tb = prmtype.toBasetype();
            switch (tb.ty)
            {
                // 6764 fix - TypeAArray may be TypeSArray have not yet run semantic().
            case Tsarray:
            case Taarray:
                {
                    // Perhaps we can do better with this, see TypeFunction.callMatch()
                    if (TypeSArray tsa = tb.isTypeSArray())
                    {
                        dinteger_t sz = tsa.dim.toInteger();
                        if (sz != fargs.length - argi)
                            return nomatch();
                    }
                    else if (TypeAArray taa = tb.isTypeAArray())
                    {
                        Expression dim = new IntegerExp(instLoc, fargs.length - argi, Type.tsize_t);

                        size_t i = templateParameterLookup(taa.index, td.parameters);
                        if (i == IDX_NOTFOUND)
                        {
                            Expression e;
                            Type t;
                            Dsymbol s;
                            Scope *sco;

                            uint errors = global.startGagging();
                            /* ref: https://issues.dlang.org/show_bug.cgi?id=11118
                             * The parameter isn't part of the template
                             * ones, let's try to find it in the
                             * instantiation scope 'sc' and the one
                             * belonging to the template itself. */
                            sco = sc;
                            taa.index.resolve(instLoc, sco, e, t, s);
                            if (!e)
                            {
                                sco = paramscope;
                                taa.index.resolve(instLoc, sco, e, t, s);
                            }
                            global.endGagging(errors);

                            if (!e)
                                return nomatch();

                            e = e.ctfeInterpret();
                            e = e.implicitCastTo(sco, Type.tsize_t);
                            e = e.optimize(WANTvalue);
                            if (!dim.equals(e))
                                return nomatch();
                        }
                        else
                        {
                            // This code matches code in TypeInstance.deduceType()
                            TemplateParameter tprm = (*td.parameters)[i];
                            TemplateValueParameter tvp = tprm.isTemplateValueParameter();
                            if (!tvp)
                                return nomatch();
                            Expression e = cast(Expression)(*dedtypes)[i];
                            if (e)
                            {
                                if (!dim.equals(e))
                                    return nomatch();
                            }
                            else
                            {
                                Type vt = tvp.valType.typeSemantic(Loc.initial, sc);
                                MATCH m = dim.implicitConvTo(vt);
                                if (m == MATCH.nomatch)
                                    return nomatch();
                                (*dedtypes)[i] = dim;
                            }
                        }
                    }
                    goto case Tarray;
                }
            case Tarray:
                {
                    TypeArray ta = cast(TypeArray)tb;
                    Type tret = fparam.isLazyArray();
                    for (; argi < fargs.length; argi++)
                    {
                        Expression arg = fargs[argi];
                        assert(arg);

                        MATCH m;
                        /* If lazy array of delegates,
                         * convert arg(s) to delegate(s)
                         */
                        if (tret)
                        {
                            if (ta.next.equals(arg.type))
                            {
                                m = MATCH.exact;
                            }
                            else
                            {
                                m = arg.implicitConvTo(tret);
                                if (m == MATCH.nomatch)
                                {
                                    if (tret.toBasetype().ty == Tvoid)
                                        m = MATCH.convert;
                                }
                            }
                        }
                        else
                        {
                            uint wm = 0;
                            m = deduceType(arg, paramscope, ta.next, *td.parameters, *dedtypes, &wm, inferStart);
                            inoutMatch |= wm;
                        }
                        if (m == MATCH.nomatch)
                            return nomatch();
                        if (m < match)
                            match = m;
                    }
                    goto Lmatch;
                }
            case Tclass:
            case Tident:
                goto Lmatch;

            default:
                return nomatch();
            }
            assert(0);
        }
        // printf(". argi = %d, nfargs = %d, nfargs2 = %d, argsConsumed = %d\n", cast(int) argi, cast(int) nfargs, cast(int) nfargs2, cast(int) argsConsumed);
        if (argsConsumed != nfargs2 && fparameters.varargs == VarArg.none)
            return nomatch();
    }

Lmatch:
    foreach (ref dedtype; *dedtypes)
    {
        if (Type at = isType(dedtype))
        {
            if (at.ty == Tnone)
            {
                TypeDeduced xt = cast(TypeDeduced)at;
                at = xt.tded; // 'unbox'
            }
            dedtype = at.merge2();
        }
    }
    for (size_t i = ntargs; i < dedargs.length; i++)
    {
        TemplateParameter tparam = (*td.parameters)[i];
        //printf("tparam[%d] = %s\n", i, tparam.ident.toChars());

        /* For T:T*, the dedargs is the T*, dedtypes is the T
         * But for function templates, we really need them to match
         */
        RootObject oarg = (*dedargs)[i];
        RootObject oded = (*dedtypes)[i];
        //printf("1dedargs[%d] = %p, dedtypes[%d] = %p\n", i, oarg, i, oded);
        //if (oarg) printf("oarg: %s\n", oarg.toChars());
        //if (oded) printf("oded: %s\n", oded.toChars());
        if (oarg)
            continue;

        if (oded)
        {
            if (tparam.specialization() || !tparam.isTemplateTypeParameter())
            {
                /* The specialization can work as long as afterwards
                 * the oded == oarg
                 */
                (*dedargs)[i] = oded;
                MATCH m2 = tparam.matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, null);
                //printf("m2 = %d\n", m2);
                if (m2 == MATCH.nomatch)
                    return nomatch();
                if (m2 < matchTiargs)
                    matchTiargs = m2; // pick worst match
                if (!(*dedtypes)[i].equals(oded))
                    .error(td.loc, "%s `%s` specialization not allowed for deduced parameter `%s`", td.kind, td.toPrettyChars, tparam.ident.toChars());
            }
            else
            {
                // Discussion: https://issues.dlang.org/show_bug.cgi?id=16484
                if (MATCH.convert < matchTiargs)
                    matchTiargs = MATCH.convert;
            }
        }
        else
        {
            oded = tparam.defaultArg(instLoc, paramscope);
            if (!oded)
            {
                // if tuple parameter and
                // tuple parameter was not in function parameter list and
                // we're one or more arguments short (i.e. no tuple argument)
                if (tparam == tp &&
                    fptupindex == IDX_NOTFOUND &&
                    ntargs <= dedargs.length - 1)
                {
                    // make tuple argument an empty tuple
                    oded = new Tuple();
                }
                else
                    return nomatch();
            }
            if (isError(oded))
                return matcherror();
            ntargs++;

            /* At the template parameter T, the picked default template argument
             * X!int should be matched to T in order to deduce dependent
             * template parameter A.
             *  auto foo(T : X!A = X!int, A...)() { ... }
             *  foo();  // T <-- X!int, A <-- (int)
             */
            if (tparam.specialization())
            {
                (*dedargs)[i] = oded;
                MATCH m2 = tparam.matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, null);
                //printf("m2 = %d\n", m2);
                if (m2 == MATCH.nomatch)
                    return nomatch();
                if (m2 < matchTiargs)
                    matchTiargs = m2; // pick worst match
                if (!(*dedtypes)[i].equals(oded))
                    .error(td.loc, "%s `%s` specialization not allowed for deduced parameter `%s`", td.kind, td.toPrettyChars, tparam.ident.toChars());
            }
        }
        oded = td.declareParameter(paramscope, tparam, oded);
        (*dedargs)[i] = oded;
    }

    /* https://issues.dlang.org/show_bug.cgi?id=7469
     * As same as the code for 7469 in findBestMatch,
     * expand a Tuple in dedargs to normalize template arguments.
     */
    if (auto d = dedargs.length)
    {
        if (auto va = isTuple((*dedargs)[d - 1]))
        {
            dedargs.setDim(d - 1);
            dedargs.insert(d - 1, &va.objects);
        }
    }
    ti.tiargs = dedargs; // update to the normalized template arguments.

    // Partially instantiate function for constraint and fd.leastAsSpecialized()
    {
        assert(paramscope.scopesym);
        Scope* sc2 = td._scope;
        sc2 = sc2.push(paramscope.scopesym);
        sc2 = sc2.push(ti);
        sc2.parent = ti;
        sc2.tinst = ti;
        sc2.minst = sc.minst;
        sc2.stc |= fd.storage_class & STC.deprecated_;

        fd = doHeaderInstantiation(td, ti, sc2, fd, tthis, argumentList);
        sc2 = sc2.pop();
        sc2 = sc2.pop();

        if (!fd)
            return nomatch();
    }

    if (td.constraint)
    {
        if (!evaluateConstraint(td, ti, sc, paramscope, dedargs, fd))
            return nomatch();
    }

    version (none)
    {
        for (size_t i = 0; i < dedargs.length; i++)
        {
            RootObject o = (*dedargs)[i];
            printf("\tdedargs[%d] = %d, %s\n", i, o.dyncast(), o.toChars());
        }
    }

    paramscope.pop();
    //printf("\tmatch %d\n", match);
    return MATCHpair(matchTiargs, match);
}

/*************************************************
 * Limited function template instantiation for using fd.leastAsSpecialized()
 */
private
FuncDeclaration doHeaderInstantiation(TemplateDeclaration td, TemplateInstance ti, Scope* sc2, FuncDeclaration fd, Type tthis, ArgumentList inferenceArguments)
{
    assert(fd);
    version (none)
    {
        printf("doHeaderInstantiation this = %s\n", toChars());
    }

    // function body and contracts are not need
    if (fd.isCtorDeclaration())
        fd = new CtorDeclaration(fd.loc, fd.endloc, fd.storage_class, fd.type.syntaxCopy());
    else
        fd = new FuncDeclaration(fd.loc, fd.endloc, fd.ident, fd.storage_class, fd.type.syntaxCopy());
    fd.parent = ti;

    assert(fd.type.ty == Tfunction);
    auto tf = fd.type.isTypeFunction();
    tf.inferenceArguments = inferenceArguments;

    if (tthis)
    {
        // Match 'tthis' to any TemplateThisParameter's
        bool hasttp = false;
        foreach (tp; *td.parameters)
        {
            TemplateThisParameter ttp = tp.isTemplateThisParameter();
            if (ttp)
                hasttp = true;
        }
        if (hasttp)
        {
            tf = tf.addSTC(ModToStc(tthis.mod)).isTypeFunction();
            assert(!tf.deco);
        }
    }

    Scope* scx = sc2.push();

    // Shouldn't run semantic on default arguments and return type.
    foreach (ref params; *tf.parameterList.parameters)
        params.defaultArg = null;
    tf.incomplete = true;

    if (fd.isCtorDeclaration())
    {
        // For constructors, emitting return type is necessary for
        // isReturnIsolated() in functionResolve.
        tf.isctor = true;

        Dsymbol parent = td.toParentDecl();
        Type tret;
        AggregateDeclaration ad = parent.isAggregateDeclaration();
        if (!ad || parent.isUnionDeclaration())
        {
            tret = Type.tvoid;
        }
        else
        {
            tret = ad.handleType();
            assert(tret);
            tret = tret.addStorageClass(fd.storage_class | scx.stc);
            tret = tret.addMod(tf.mod);
        }
        tf.next = tret;
        if (ad && ad.isStructDeclaration())
            tf.isref = 1;
        //printf("tf = %s\n", tf.toChars());
    }
    else
        tf.next = null;
    fd.type = tf;
    fd.type = fd.type.addSTC(scx.stc);
    fd.type = fd.type.typeSemantic(fd.loc, scx);
    scx = scx.pop();

    if (fd.type.ty != Tfunction)
        return null;

    fd.originalType = fd.type; // for mangling
    //printf("\t[%s] fd.type = %s, mod = %x, ", loc.toChars(), fd.type.toChars(), fd.type.mod);
    //printf("fd.needThis() = %d\n", fd.needThis());

    return fd;
}

/**************************************************
 * Declare template parameter tp with value o, and install it in the scope sc.
 */
extern (D) RootObject declareParameter(TemplateDeclaration td, Scope* sc, TemplateParameter tp, RootObject o)
{
    //printf("TemplateDeclaration.declareParameter('%s', o = %p)\n", tp.ident.toChars(), o);
    Type ta = isType(o);
    Expression ea = isExpression(o);
    Dsymbol sa = isDsymbol(o);
    Tuple va = isTuple(o);

    Declaration d;
    VarDeclaration v = null;

    if (ea)
    {
        if (ea.op == EXP.type)
            ta = ea.type;
        else if (auto se = ea.isScopeExp())
            sa = se.sds;
        else if (auto te = ea.isThisExp())
            sa = te.var;
        else if (auto se = ea.isSuperExp())
            sa = se.var;
        else if (auto fe = ea.isFuncExp())
        {
            if (fe.td)
                sa = fe.td;
            else
                sa = fe.fd;
        }
    }

    if (ta)
    {
        //printf("type %s\n", ta.toChars());
        auto ad = new AliasDeclaration(Loc.initial, tp.ident, ta);
        ad.storage_class |= STC.templateparameter;
        d = ad;
    }
    else if (sa)
    {
        //printf("Alias %s %s;\n", sa.ident.toChars(), tp.ident.toChars());
        auto ad = new AliasDeclaration(Loc.initial, tp.ident, sa);
        ad.storage_class |= STC.templateparameter;
        d = ad;
    }
    else if (ea)
    {
        // tdtypes.data[i] always matches ea here
        Initializer _init = new ExpInitializer(td.loc, ea);
        TemplateValueParameter tvp = tp.isTemplateValueParameter();
        Type t = tvp ? tvp.valType : null;
        v = new VarDeclaration(td.loc, t, tp.ident, _init);
        v.storage_class = STC.manifest | STC.templateparameter;
        d = v;
    }
    else if (va)
    {
        //printf("\ttuple\n");
        d = new TupleDeclaration(td.loc, tp.ident, &va.objects);
    }
    else
    {
        assert(0);
    }
    d.storage_class |= STC.templateparameter;

    if (ta)
    {
        Type t = ta;
        // consistent with Type.checkDeprecated()
        while (t.ty != Tenum)
        {
            if (!t.nextOf())
                break;
            t = (cast(TypeNext)t).next;
        }
        if (Dsymbol s = t.toDsymbol(sc))
        {
            if (s.isDeprecated())
                d.storage_class |= STC.deprecated_;
        }
    }
    else if (sa)
    {
        if (sa.isDeprecated())
            d.storage_class |= STC.deprecated_;
    }

    if (!sc.insert(d))
        .error(td.loc, "%s `%s` declaration `%s` is already defined", td.kind, td.toPrettyChars, tp.ident.toChars());
    d.dsymbolSemantic(sc);
    /* So the caller's o gets updated with the result of semantic() being run on o
     */
    if (v)
        o = v._init.initializerToExpression();
    return o;
}

/*************************************************
 * Given function arguments, figure out which template function
 * to expand, and return matching result.
 * Params:
 *      m           = matching result
 *      dstart      = the root of overloaded function templates
 *      loc         = instantiation location
 *      sc          = instantiation scope
 *      tiargs      = initial list of template arguments
 *      tthis       = if !NULL, the 'this' pointer argument
 *      argumentList= arguments to function
 *      errorHelper = delegate to send error message to if not null
 */
void functionResolve(ref MatchAccumulator m, Dsymbol dstart, Loc loc, Scope* sc, Objects* tiargs,
    Type tthis, ArgumentList argumentList, void delegate(const(char)*) scope errorHelper = null)
{
    version (none)
    {
        printf("functionResolve() dstart = %s\n", dstart.toChars());
        printf("    tiargs:\n");
        if (tiargs)
        {
            for (size_t i = 0; i < tiargs.length; i++)
            {
                RootObject arg = (*tiargs)[i];
                printf("\t%s\n", arg.toChars());
            }
        }
        printf("    fargs:\n");
        for (size_t i = 0; i < (fargs ? fargs.length : 0); i++)
        {
            Expression arg = (*fargs)[i];
            printf("\t%s %s\n", arg.type.toChars(), arg.toChars());
            //printf("\tty = %d\n", arg.type.ty);
        }
        //printf("stc = %llx\n", dstart._scope.stc);
        //printf("match:t/f = %d/%d\n", ta_last, m.last);
    }

    // results
    int property = 0;   // 0: uninitialized
                        // 1: seen @property
                        // 2: not @property
    size_t ov_index = 0;
    TemplateDeclaration td_best;
    TemplateInstance ti_best;
    MATCH ta_last = m.last != MATCH.nomatch ? MATCH.exact : MATCH.nomatch;
    Type tthis_best;

    int applyFunction(FuncDeclaration fd)
    {
        // skip duplicates
        if (fd == m.lastf)
            return 0;
        // explicitly specified tiargs never match to non template function
        if (tiargs && tiargs.length > 0)
            return 0;

        // constructors need a valid scope in order to detect semantic errors
        if (!fd.isCtorDeclaration &&
            fd.semanticRun < PASS.semanticdone)
        {
            fd.ungagSpeculative();
            fd.dsymbolSemantic(null);
        }
        if (fd.semanticRun < PASS.semanticdone)
        {
            .error(loc, "forward reference to template `%s`", fd.toChars());
            return 1;
        }
        //printf("fd = %s %s, fargs = %s\n", fd.toChars(), fd.type.toChars(), fargs.toChars());
        auto tf = fd.type.isTypeFunction();

        int prop = tf.isproperty ? 1 : 2;
        if (property == 0)
            property = prop;
        else if (property != prop)
            error(fd.loc, "cannot overload both property and non-property functions");

        /* For constructors, qualifier check will be opposite direction.
         * Qualified constructor always makes qualified object, then will be checked
         * that it is implicitly convertible to tthis.
         */
        Type tthis_fd = fd.needThis() ? tthis : null;
        bool isCtorCall = tthis_fd && fd.isCtorDeclaration();
        if (isCtorCall)
        {
            //printf("%s tf.mod = x%x tthis_fd.mod = x%x %d\n", tf.toChars(),
            //        tf.mod, tthis_fd.mod, fd.isReturnIsolated());
            if (MODimplicitConv(tf.mod, tthis_fd.mod) ||
                tf.isWild() && tf.isShared() == tthis_fd.isShared() ||
                fd.isReturnIsolated())
            {
                /* && tf.isShared() == tthis_fd.isShared()*/
                // Uniquely constructed object can ignore shared qualifier.
                // TODO: Is this appropriate?
                tthis_fd = null;
            }
            else
                return 0;   // MATCH.nomatch
        }
        /* Fix Issue 17970:
           If a struct is declared as shared the dtor is automatically
           considered to be shared, but when the struct is instantiated
           the instance is no longer considered to be shared when the
           function call matching is done. The fix makes it so that if a
           struct declaration is shared, when the destructor is called,
           the instantiated struct is also considered shared.
        */
        if (auto dt = fd.isDtorDeclaration())
        {
            auto dtmod = dt.type.toTypeFunction();
            auto shared_dtor = dtmod.mod & MODFlags.shared_;
            auto shared_this = tthis_fd !is null ?
                tthis_fd.mod & MODFlags.shared_ : 0;
            if (shared_dtor && !shared_this)
                tthis_fd = dtmod;
            else if (shared_this && !shared_dtor && tthis_fd !is null)
                tf.mod = tthis_fd.mod;
        }
        const(char)* failMessage;
        MATCH mfa = tf.callMatch(tthis_fd, argumentList, 0, errorHelper, sc);
        //printf("test1: mfa = %d\n", mfa);
        if (failMessage)
            errorHelper(failMessage);
        if (mfa == MATCH.nomatch)
            return 0;

        int firstIsBetter()
        {
            td_best = null;
            ti_best = null;
            ta_last = MATCH.exact;
            m.last = mfa;
            m.lastf = fd;
            tthis_best = tthis_fd;
            ov_index = 0;
            m.count = 1;
            return 0;
        }

        if (mfa > m.last) return firstIsBetter();
        if (mfa < m.last) return 0;

        /* See if one of the matches overrides the other.
         */
        assert(m.lastf);
        if (m.lastf.overrides(fd)) return 0;
        if (fd.overrides(m.lastf)) return firstIsBetter();

        /* Try to disambiguate using template-style partial ordering rules.
         * In essence, if f() and g() are ambiguous, if f() can call g(),
         * but g() cannot call f(), then pick f().
         * This is because f() is "more specialized."
         */
        {
            MATCH c1 = FuncDeclaration.leastAsSpecialized(fd, m.lastf, argumentList.names);
            MATCH c2 = FuncDeclaration.leastAsSpecialized(m.lastf, fd, argumentList.names);
            //printf("c1 = %d, c2 = %d\n", c1, c2);
            if (c1 > c2) return firstIsBetter();
            if (c1 < c2) return 0;
        }

        /* The 'overrides' check above does covariant checking only
         * for virtual member functions. It should do it for all functions,
         * but in order to not risk breaking code we put it after
         * the 'leastAsSpecialized' check.
         * In the future try moving it before.
         * I.e. a not-the-same-but-covariant match is preferred,
         * as it is more restrictive.
         */
        if (!m.lastf.type.equals(fd.type))
        {
            //printf("cov: %d %d\n", m.lastf.type.covariant(fd.type), fd.type.covariant(m.lastf.type));
            const lastCovariant = m.lastf.type.covariant(fd.type);
            const firstCovariant = fd.type.covariant(m.lastf.type);

            if (lastCovariant == Covariant.yes || lastCovariant == Covariant.no)
            {
                if (firstCovariant != Covariant.yes && firstCovariant != Covariant.no)
                {
                    return 0;
                }
            }
            else if (firstCovariant == Covariant.yes || firstCovariant == Covariant.no)
            {
                return firstIsBetter();
            }
        }

        /* If the two functions are the same function, like:
         *    int foo(int);
         *    int foo(int x) { ... }
         * then pick the one with the body.
         *
         * If none has a body then don't care because the same
         * real function would be linked to the decl (e.g from object file)
         */
        if (tf.equals(m.lastf.type) &&
            fd.storage_class == m.lastf.storage_class &&
            fd.parent == m.lastf.parent &&
            fd.visibility == m.lastf.visibility &&
            fd._linkage == m.lastf._linkage)
        {
            if (fd.fbody && !m.lastf.fbody)
                return firstIsBetter();
            if (!fd.fbody)
                return 0;
        }

        // https://issues.dlang.org/show_bug.cgi?id=14450
        // Prefer exact qualified constructor for the creating object type
        if (isCtorCall && tf.mod != m.lastf.type.mod)
        {
            if (tthis.mod == tf.mod) return firstIsBetter();
            if (tthis.mod == m.lastf.type.mod) return 0;
        }

        m.nextf = fd;
        m.count++;
        return 0;
    }

    int applyTemplate(TemplateDeclaration td)
    {
        //printf("applyTemplate(): td = %s\n", td.toChars());
        if (td == td_best)   // skip duplicates
            return 0;

        if (!sc)
            sc = td._scope; // workaround for Type.aliasthisOf

        if (td.semanticRun == PASS.initial && td._scope)
        {
            // Try to fix forward reference. Ungag errors while doing so.
            td.ungagSpeculative();
            td.dsymbolSemantic(td._scope);
        }
        if (td.semanticRun == PASS.initial)
        {
            .error(loc, "forward reference to template `%s`", td.toChars());
        Lerror:
            m.lastf = null;
            m.count = 0;
            m.last = MATCH.nomatch;
            return 1;
        }
        //printf("td = %s\n", td.toChars());

        auto f = td.onemember ? td.onemember.isFuncDeclaration() : null;
        if (!f)
        {
            if (!tiargs)
                tiargs = new Objects();
            auto ti = new TemplateInstance(loc, td, tiargs);
            Objects dedtypes = Objects(td.parameters.length);
            assert(td.semanticRun != PASS.initial);
            MATCH mta = matchWithInstance(sc, td, ti, dedtypes, argumentList, 0);
            //printf("matchWithInstance = %d\n", mta);
            if (mta == MATCH.nomatch || mta < ta_last)   // no match or less match
                return 0;

            ti.templateInstanceSemantic(sc, argumentList);
            if (!ti.inst)               // if template failed to expand
                return 0;

            Dsymbol s = ti.inst.toAlias();
            FuncDeclaration fd;
            if (auto tdx = s.isTemplateDeclaration())
            {
                Objects dedtypesX;      // empty tiargs

                // https://issues.dlang.org/show_bug.cgi?id=11553
                // Check for recursive instantiation of tdx.
                for (TemplatePrevious* p = tdx.previous; p; p = p.prev)
                {
                    if (arrayObjectMatch(*p.dedargs, dedtypesX))
                    {
                        //printf("recursive, no match p.sc=%p %p %s\n", p.sc, this, this.toChars());
                        /* It must be a subscope of p.sc, other scope chains are not recursive
                         * instantiations.
                         */
                        for (Scope* scx = sc; scx; scx = scx.enclosing)
                        {
                            if (scx == p.sc)
                            {
                                error(loc, "recursive template expansion while looking for `%s.%s`", ti.toChars(), tdx.toChars());
                                goto Lerror;
                            }
                        }
                    }
                    /* BUG: should also check for ref param differences
                     */
                }

                TemplatePrevious pr;
                pr.prev = tdx.previous;
                pr.sc = sc;
                pr.dedargs = &dedtypesX;
                tdx.previous = &pr;             // add this to threaded list

                fd = resolveFuncCall(loc, sc, s, null, tthis, argumentList, FuncResolveFlag.quiet);

                tdx.previous = pr.prev;         // unlink from threaded list
            }
            else if (s.isFuncDeclaration())
            {
                fd = resolveFuncCall(loc, sc, s, null, tthis, argumentList, FuncResolveFlag.quiet);
            }
            else
                goto Lerror;

            if (!fd)
                return 0;

            if (fd.type.ty != Tfunction)
            {
                m.lastf = fd;   // to propagate "error match"
                m.count = 1;
                m.last = MATCH.nomatch;
                return 1;
            }

            Type tthis_fd = fd.needThis() && !fd.isCtorDeclaration() ? tthis : null;

            auto tf = fd.type.isTypeFunction();
            MATCH mfa = tf.callMatch(tthis_fd, argumentList, 0, null, sc);
            if (mfa < m.last)
                return 0;

            if (mta < ta_last) goto Ltd_best2;
            if (mta > ta_last) goto Ltd2;

            if (mfa < m.last) goto Ltd_best2;
            if (mfa > m.last) goto Ltd2;

            // td_best and td are ambiguous
            //printf("Lambig2\n");
            m.nextf = fd;
            m.count++;
            return 0;

        Ltd_best2:
            return 0;

        Ltd2:
            // td is the new best match
            assert(td._scope);
            td_best = td;
            ti_best = null;
            property = 0;   // (backward compatibility)
            ta_last = mta;
            m.last = mfa;
            m.lastf = fd;
            tthis_best = tthis_fd;
            ov_index = 0;
            m.nextf = null;
            m.count = 1;
            return 0;
        }

        //printf("td = %s\n", td.toChars());
        for (size_t ovi = 0; f; f = f.overnext0, ovi++)
        {
            if (f.type.ty != Tfunction || f.errors)
                goto Lerror;

            /* This is a 'dummy' instance to evaluate constraint properly.
             */
            auto ti = new TemplateInstance(loc, td, tiargs);
            ti.parent = td.parent;  // Maybe calculating valid 'enclosing' is unnecessary.

            auto fd = f;
            MATCHpair x = td.deduceFunctionTemplateMatch(ti, sc, fd, tthis, argumentList);
            MATCH mta = x.mta;
            MATCH mfa = x.mfa;
            //printf("match:t/f = %d/%d\n", mta, mfa);
            if (!fd || mfa == MATCH.nomatch)
                continue;

            Type tthis_fd = fd.needThis() ? tthis : null;

            bool isCtorCall = tthis_fd && fd.isCtorDeclaration();
            if (isCtorCall)
            {
                // Constructor call requires additional check.
                auto tf = fd.type.isTypeFunction();
                assert(tf.next);
                if (MODimplicitConv(tf.mod, tthis_fd.mod) ||
                    tf.isWild() && tf.isShared() == tthis_fd.isShared() ||
                    fd.isReturnIsolated())
                {
                    tthis_fd = null;
                }
                else
                    continue;   // MATCH.nomatch

                // need to check here whether the constructor is the member of a struct
                // declaration that defines a copy constructor. This is already checked
                // in the semantic of CtorDeclaration, however, when matching functions,
                // the template instance is not expanded.
                // https://issues.dlang.org/show_bug.cgi?id=21613
                auto ad = fd.isThis();
                auto sd = ad.isStructDeclaration();
                if (checkHasBothRvalueAndCpCtor(sd, fd.isCtorDeclaration(), ti))
                    continue;
            }

            if (mta < ta_last) goto Ltd_best;
            if (mta > ta_last) goto Ltd;

            if (mfa < m.last) goto Ltd_best;
            if (mfa > m.last) goto Ltd;

            if (td_best)
            {
                // Disambiguate by picking the most specialized TemplateDeclaration
                MATCH c1 = leastAsSpecialized(sc, td, td_best, argumentList);
                MATCH c2 = leastAsSpecialized(sc, td_best, td, argumentList);
                //printf("1: c1 = %d, c2 = %d\n", c1, c2);
                if (c1 > c2) goto Ltd;
                if (c1 < c2) goto Ltd_best;
            }
            assert(fd && m.lastf);
            {
                // Disambiguate by tf.callMatch
                auto tf1 = fd.type.isTypeFunction();
                auto tf2 = m.lastf.type.isTypeFunction();
                MATCH c1 = tf1.callMatch(tthis_fd, argumentList, 0, null, sc);
                MATCH c2 = tf2.callMatch(tthis_best, argumentList, 0, null, sc);
                //printf("2: c1 = %d, c2 = %d\n", c1, c2);
                if (c1 > c2) goto Ltd;
                if (c1 < c2) goto Ltd_best;
            }
            {
                // Disambiguate by picking the most specialized FunctionDeclaration
                MATCH c1 = FuncDeclaration.leastAsSpecialized(fd, m.lastf, argumentList.names);
                MATCH c2 = FuncDeclaration.leastAsSpecialized(m.lastf, fd, argumentList.names);
                //printf("3: c1 = %d, c2 = %d\n", c1, c2);
                if (c1 > c2) goto Ltd;
                if (c1 < c2) goto Ltd_best;
            }

            // https://issues.dlang.org/show_bug.cgi?id=14450
            // Prefer exact qualified constructor for the creating object type
            if (isCtorCall && fd.type.mod != m.lastf.type.mod)
            {
                if (tthis.mod == fd.type.mod) goto Ltd;
                if (tthis.mod == m.lastf.type.mod) goto Ltd_best;
            }

            m.nextf = fd;
            m.count++;
            continue;

        Ltd_best:           // td_best is the best match so far
            //printf("Ltd_best\n");
            continue;

        Ltd:                // td is the new best match
            //printf("Ltd\n");
            assert(td._scope);
            td_best = td;
            ti_best = ti;
            property = 0;   // (backward compatibility)
            ta_last = mta;
            m.last = mfa;
            m.lastf = fd;
            tthis_best = tthis_fd;
            ov_index = ovi;
            m.nextf = null;
            m.count = 1;
            continue;
        }
        return 0;
    }

    auto td = dstart.isTemplateDeclaration();
    if (td && td.funcroot)
        dstart = td.funcroot;
    overloadApply(dstart, (Dsymbol s)
    {
        if (s.errors)
            return 0;
        if (auto fd = s.isFuncDeclaration())
            return applyFunction(fd);
        if (auto td = s.isTemplateDeclaration())
            return applyTemplate(td);
        return 0;
    }, sc);

    //printf("td_best = %p, m.lastf = %p\n", td_best, m.lastf);
    if (td_best && ti_best && m.count == 1)
    {
        // Matches to template function
        assert(td_best.onemember && td_best.onemember.isFuncDeclaration());
        /* The best match is td_best with arguments tdargs.
         * Now instantiate the template.
         */
        assert(td_best._scope);
        if (!sc)
            sc = td_best._scope; // workaround for Type.aliasthisOf

        auto ti = new TemplateInstance(loc, td_best, ti_best.tiargs);
        ti.templateInstanceSemantic(sc, argumentList);

        m.lastf = ti.toAlias().isFuncDeclaration();
        if (!m.lastf)
            goto Lnomatch;
        if (ti.errors)
        {
        Lerror:
            m.count = 1;
            assert(m.lastf);
            m.last = MATCH.nomatch;
            return;
        }

        // look forward instantiated overload function
        // Dsymbol.oneMembers is alredy called in TemplateInstance.semantic.
        // it has filled overnext0d
        while (ov_index--)
        {
            m.lastf = m.lastf.overnext0;
            assert(m.lastf);
        }

        tthis_best = m.lastf.needThis() && !m.lastf.isCtorDeclaration() ? tthis : null;

        if (m.lastf.type.ty == Terror)
            goto Lerror;
        auto tf = m.lastf.type.isTypeFunction();
        if (!tf.callMatch(tthis_best, argumentList, 0, null, sc))
            goto Lnomatch;

        /* As https://issues.dlang.org/show_bug.cgi?id=3682 shows,
         * a template instance can be matched while instantiating
         * that same template. Thus, the function type can be incomplete. Complete it.
         *
         * https://issues.dlang.org/show_bug.cgi?id=9208
         * For auto function, completion should be deferred to the end of
         * its semantic3. Should not complete it in here.
         */
        if (tf.next && !m.lastf.inferRetType)
        {
            m.lastf.type = tf.typeSemantic(loc, sc);
        }
    }
    else if (m.lastf)
    {
        // Matches to non template function,
        // or found matches were ambiguous.
        assert(m.count >= 1);
    }
    else
    {
    Lnomatch:
        m.count = 0;
        m.lastf = null;
        m.last = MATCH.nomatch;
    }
}
