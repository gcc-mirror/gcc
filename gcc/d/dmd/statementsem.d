/**
 * Does semantic analysis for statements.
 *
 * Specification: $(LINK2 https://dlang.org/spec/statement.html, Statements)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/statementsem.d, _statementsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_statementsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/statementsem.d
 */

module dmd.statementsem;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.arrayop;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.blockexit;
import dmd.clone;
import dmd.cond;
import dmd.ctorflow;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.iasm;
import dmd.id;
import dmd.identifier;
import dmd.importc;
import dmd.init;
import dmd.intrange;
import dmd.location;
import dmd.mtype;
import dmd.mustuse;
import dmd.nogc;
import dmd.optimize;
import dmd.opover;
import dmd.parse;
import dmd.common.outbuffer;
import dmd.root.string;
import dmd.safe : isSafe, isSaferD, setUnsafe;
import dmd.semantic2;
import dmd.sideeffect;
import dmd.statement;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

version (DMDLIB)
{
    version = CallbackAPI;
}

/*****************************************
 * CTFE requires FuncDeclaration::labtab for the interpretation.
 * So fixing the label name inside in/out contracts is necessary
 * for the uniqueness in labtab.
 * Params:
 *      sc = context
 *      ident = statement label name to be adjusted
 * Returns:
 *      adjusted label name
 */
private Identifier fixupLabelName(Scope* sc, Identifier ident)
{
    Contract c = sc.contract;
    const id = ident.toString();
    if (c != Contract.none && c != Contract.invariant_ &&
        !(id.length >= 2 && id[0] == '_' && id[1] == '_'))  // does not start with "__"
    {
        OutBuffer buf;
        buf.writestring(c == Contract.require ? "__in_" : "__out_");
        buf.writestring(ident.toString());

        ident = Identifier.idPool(buf[]);
    }
    return ident;
}

/*******************************************
 * Check to see if statement is the innermost labeled statement.
 * Params:
 *      sc = context
 *      statement = Statement to check
 * Returns:
 *      if `true`, then the `LabelStatement`, otherwise `null`
 */
private LabelStatement checkLabeledLoop(Scope* sc, Statement statement) @safe
{
    if (sc.slabel && sc.slabel.statement == statement)
    {
        return sc.slabel;
    }
    return null;
}

/***********************************************************
 * Check an assignment is used as a condition.
 * Intended to be use before the `semantic` call on `e`.
 * Params:
 *  e = condition expression which is not yet run semantic analysis.
 * Returns:
 *  `e` or ErrorExp.
 */
private Expression checkAssignmentAsCondition(Expression e, Scope* sc)
{
    if (sc.inCfile)
        return e;
    auto ec = lastComma(e);
    if (ec.op == EXP.assign)
    {
        error(ec.loc, "assignment cannot be used as a condition, perhaps `==` was meant?");
        return ErrorExp.get();
    }
    return e;
}

// Performs semantic analysis in Statement AST nodes
Statement statementSemantic(Statement s, Scope* sc)
{
    import dmd.compiler;

    version (CallbackAPI)
        Compiler.onStatementSemanticStart(s, sc);

    Statement result = statementSemanticVisit(s, sc);

    version (CallbackAPI)
        Compiler.onStatementSemanticDone(s, sc);

    return result;
}

package (dmd)
Statement statementSemanticVisit(Statement s, Scope* sc)
{
    Statement result;

    void setError()
    {
        result = new ErrorStatement();
    }

    void visitDefaultCase(Statement s)
    {
        result = s;
    }

    void visitError(ErrorStatement s)
    {
        result = s;
    }

    void visitPeel(PeelStatement s)
    {
        /* "peel" off this wrapper, and don't run semantic()
         * on the result.
         */
        result = s.s;
    }

    void visitExp(ExpStatement s)
    {
        /* https://dlang.org/spec/statement.html#expression-statement
         */

        if (!s.exp)
        {
            result = s;
            return;
        }
        //printf("ExpStatement::semantic() %s\n", exp.toChars());

        // Allow CommaExp in ExpStatement because return isn't used
        CommaExp.allow(s.exp);

        s.exp = s.exp.expressionSemantic(sc);
        s.exp = resolveProperties(sc, s.exp);
        s.exp = s.exp.addDtorHook(sc);
        if (checkNonAssignmentArrayOp(s.exp))
            s.exp = ErrorExp.get();
        if (auto f = isFuncAddress(s.exp))
        {
            if (f.checkForwardRef(s.exp.loc))
                s.exp = ErrorExp.get();
        }
        if (checkMustUse(s.exp, sc))
            s.exp = ErrorExp.get();
        if (!sc.inCfile && discardValue(s.exp))
            s.exp = ErrorExp.get();

        s.exp = s.exp.optimize(WANTvalue);
        s.exp = checkGC(sc, s.exp);
        if (s.exp.op == EXP.error)
            return setError();
        result = s;
    }

    void visitDtorExp(DtorExpStatement s)
    {
        visitExp(s);
    }

    void visitMixin(MixinStatement cs)
    {
        /* https://dlang.org/spec/statement.html#mixin-statement
         */

        //printf("MixinStatement::semantic() %s\n", exp.toChars());
        Statements* a = cs.flatten(sc);
        if (!a)
            return;
        Statement s = new CompoundStatement(cs.loc, a);
        result = s.statementSemantic(sc);
    }

    void visitCompound(CompoundStatement cs)
    {
        //printf("CompoundStatement::semantic(this = %p, sc = %p)\n", cs, sc);
        version (none)
        {
            foreach (i, s; cs.statements)
            {
                if (s)
                    printf("[%d]: %s", i, s.toChars());
            }
        }

        for (size_t i = 0; i < cs.statements.length;)
        {
            Statement s = (*cs.statements)[i];
            if (!s)
            {
                ++i;
                continue;
            }

            if (auto flt = s.flatten(sc))
            {
                cs.statements.remove(i);
                cs.statements.insert(i, flt);
                continue;
            }
            s = s.statementSemantic(sc);
            (*cs.statements)[i] = s;
            if (!s)
            {
                /* Remove NULL statements from the list.
                 */
                cs.statements.remove(i);
                continue;
            }
            if (s.isErrorStatement())
            {
                result = s;     // propagate error up the AST
                ++i;
                continue;       // look for errors in rest of statements
            }

            // expand tuple variables in order to attach destruction/exception logic
            if (auto es = s.isExpStatement())
            {
                if (es.exp && es.exp.isDeclarationExp())
                {
                    auto de = es.exp.isDeclarationExp();
                    auto vd = de.declaration.isVarDeclaration();
                    if (vd && vd.aliasTuple && vd.aliasTuple.objects.length)
                    {
                        auto j = i;
                        cs.statements.insert(i, vd.aliasTuple.objects.length - 1, null);
                        vd.aliasTuple.foreachVar((v) { (*cs.statements)[j++] = toStatement(v); });
                        s = (*cs.statements)[i];
                    }
                }
            }

            Statement sentry;
            Statement sexception;
            Statement sfinally;

            (*cs.statements)[i] = s.scopeCode(sc, sentry, sexception, sfinally);
            if (sentry)
            {
                sentry = sentry.statementSemantic(sc);
                cs.statements.insert(i, sentry);
                i++;
            }
            if (sexception)
                sexception = sexception.statementSemantic(sc);
            if (sexception)
            {
                /* Returns: true if statements[] are empty statements
                 */
                static bool isEmpty(const Statement[] statements)
                {
                    foreach (s; statements)
                    {
                        if (const cs = s.isCompoundStatement())
                        {
                            if (!isEmpty((*cs.statements)[]))
                                return false;
                        }
                        else
                            return false;
                    }
                    return true;
                }

                if (!sfinally && isEmpty((*cs.statements)[i + 1 .. cs.statements.length]))
                {
                }
                else
                {
                    /* Rewrite:
                     *      s; s1; s2;
                     * As:
                     *      s;
                     *      try { s1; s2; }
                     *      catch (Throwable __o)
                     *      { sexception; throw __o; }
                     */
                    auto a = new Statements();
                    a.pushSlice((*cs.statements)[i + 1 .. cs.statements.length]);
                    cs.statements.setDim(i + 1);

                    Statement _body = new CompoundStatement(Loc.initial, a);
                    _body = new ScopeStatement(Loc.initial, _body, Loc.initial);

                    Identifier id = Identifier.generateId("__o");

                    Statement handler = new PeelStatement(sexception);
                    if (sexception.blockExit(sc.func, null) & BE.fallthru)
                    {
                        auto ts = new ThrowStatement(Loc.initial, new IdentifierExp(Loc.initial, id));
                        ts.internalThrow = true;
                        handler = new CompoundStatement(Loc.initial, handler, ts);
                    }

                    auto catches = new Catches();
                    auto ctch = new Catch(Loc.initial, getThrowable(), id, handler);
                    ctch.internalCatch = true;
                    catches.push(ctch);

                    Statement st = new TryCatchStatement(Loc.initial, _body, catches);
                    if (sfinally)
                        st = new TryFinallyStatement(Loc.initial, st, sfinally);
                    st = st.statementSemantic(sc);

                    cs.statements.push(st);
                    break;
                }
            }
            else if (sfinally)
            {
                if (0 && i + 1 == cs.statements.length)
                {
                    cs.statements.push(sfinally);
                }
                else
                {
                    /* Rewrite:
                     *      s; s1; s2;
                     * As:
                     *      s; try { s1; s2; } finally { sfinally; }
                     */
                    auto a = new Statements();
                    a.pushSlice((*cs.statements)[i + 1 .. cs.statements.length]);
                    cs.statements.setDim(i + 1);

                    auto _body = new CompoundStatement(Loc.initial, a);
                    Statement stf = new TryFinallyStatement(Loc.initial, _body, sfinally);
                    stf = stf.statementSemantic(sc);
                    cs.statements.push(stf);
                    break;
                }
            }
            i++;
        }

        /* Flatten them in place
         */
        void flattenStatements(ref Statements statements)
        {
            for (size_t i = 0; i < statements.length;)
            {
                if (auto s = statements[i])
                {
                    if (auto flt = s.flatten(sc))
                    {
                        statements.remove(i);
                        statements.insert(i, flt);
                        continue;
                    }
                }
                ++i;
            }
        }

        /* https://issues.dlang.org/show_bug.cgi?id=11653
         * 'semantic' may return another CompoundStatement
         * (eg. CaseRangeStatement), so flatten it here.
         */
        flattenStatements(*cs.statements);

        foreach (s; *cs.statements)
        {
            if (!s)
                continue;

            if (auto se = s.isErrorStatement())
            {
                result = se;
                return;
            }
        }

        if (cs.statements.length == 1)
        {
            result = (*cs.statements)[0];
            return;
        }
        result = cs;
    }

    void visitUnrolledLoop(UnrolledLoopStatement uls)
    {
        //printf("UnrolledLoopStatement::semantic(this = %p, sc = %p)\n", uls, sc);
        Scope* scd = sc.push();
        scd.sbreak = uls;
        scd.scontinue = uls;

        Statement serror = null;
        foreach (i, ref s; *uls.statements)
        {
            if (s)
            {
                //printf("[%d]: %s\n", i, s.toChars());
                s = s.statementSemantic(scd);
                if (s && !serror)
                    serror = s.isErrorStatement();
            }
        }

        scd.pop();
        result = serror ? serror : uls;
    }

    void visitScope(ScopeStatement ss)
    {
        //printf("ScopeStatement::semantic(sc = %p)\n", sc);
        if (!ss.statement)
        {
            result = ss;
            return;
        }

        ScopeDsymbol sym = new ScopeDsymbol();
        sym.parent = sc.scopesym;
        sym.endlinnum = ss.endloc.linnum;
        sc = sc.push(sym);

        Statements* a = ss.statement.flatten(sc);
        if (a)
        {
            ss.statement = new CompoundStatement(ss.loc, a);
        }

        ss.statement = ss.statement.statementSemantic(sc);
        if (ss.statement)
        {
            if (ss.statement.isErrorStatement())
            {
                sc.pop();
                result = ss.statement;
                return;
            }

            Statement sentry;
            Statement sexception;
            Statement sfinally;
            ss.statement = ss.statement.scopeCode(sc, sentry, sexception, sfinally);
            assert(!sentry);
            assert(!sexception);
            if (sfinally)
            {
                //printf("adding sfinally\n");
                sfinally = sfinally.statementSemantic(sc);
                ss.statement = new CompoundStatement(ss.loc, ss.statement, sfinally);
            }
        }
        sc.pop();
        result = ss;
    }

    void visitForwarding(ForwardingStatement ss)
    {
        assert(ss.sym);
        for (Scope* csc = sc; !ss.sym.parent; csc = csc.enclosing)
        {
            assert(csc);
            ss.sym.parent = csc.scopesym;
        }
        sc = sc.push(ss.sym);
        sc.sbreak = ss;
        sc.scontinue = ss;
        ss.statement = ss.statement.statementSemantic(sc);
        sc = sc.pop();
        result = ss.statement;
    }

    void visitWhile(WhileStatement ws)
    {
        /* Rewrite as a for(;condition;) loop
         * https://dlang.org/spec/statement.html#while-statement
         */
        Expression cond = ws.condition;
        Statement _body = ws._body;
        if (ws.param)
        {
            /**
             * If the while loop is of form `while(auto a = exp) { loop_body }`,
             * rewrite to:
             *
             * while(true)
             *     if (auto a = exp)
             *     { loop_body }
             *     else
             *     { break; }
             */
            _body = new IfStatement(ws.loc, ws.param, ws.condition, ws._body, new BreakStatement(ws.loc, null), ws.endloc);
            cond = IntegerExp.createBool(true);
        }
        Statement s = new ForStatement(ws.loc, null, cond, null, _body, ws.endloc);
        s = s.statementSemantic(sc);
        result = s;
    }

    void visitDo(DoStatement ds)
    {
        /* https://dlang.org/spec/statement.html#do-statement
         */
        const inLoopSave = sc.inLoop;
        sc.inLoop = true;
        if (ds._body)
            ds._body = ds._body.semanticScope(sc, ds, ds, null);
        sc.inLoop = inLoopSave;

        if (auto dotid = ds.condition.isDotIdExp())
            dotid.noderef = true;

        // check in syntax level
        ds.condition = checkAssignmentAsCondition(ds.condition, sc);

        ds.condition = ds.condition.expressionSemantic(sc);
        ds.condition = resolveProperties(sc, ds.condition);
        if (checkNonAssignmentArrayOp(ds.condition))
            ds.condition = ErrorExp.get();
        ds.condition = ds.condition.optimize(WANTvalue);
        ds.condition = checkGC(sc, ds.condition);

        ds.condition = ds.condition.toBoolean(sc);

        if (ds.condition.op == EXP.error)
            return setError();
        if (ds._body && ds._body.isErrorStatement())
        {
            result = ds._body;
            return;
        }

        result = ds;
    }

    void visitFor(ForStatement fs)
    {
        /* https://dlang.org/spec/statement.html#for-statement
         */
        //printf("ForStatement::semantic %s\n", fs.toChars());

        if (fs._init)
        {
            /* Rewrite:
             *  for (auto v1 = i1, v2 = i2; condition; increment) { ... }
             * to:
             *  { auto v1 = i1, v2 = i2; for (; condition; increment) { ... } }
             * then lowered to:
             *  auto v1 = i1;
             *  try {
             *    auto v2 = i2;
             *    try {
             *      for (; condition; increment) { ... }
             *    } finally { v2.~this(); }
             *  } finally { v1.~this(); }
             */
            auto ainit = new Statements();
            ainit.push(fs._init);
            fs._init = null;
            ainit.push(fs);
            Statement s = new CompoundStatement(fs.loc, ainit);
            s = new ScopeStatement(fs.loc, s, fs.endloc);
            s = s.statementSemantic(sc);
            if (!s.isErrorStatement())
            {
                if (LabelStatement ls = checkLabeledLoop(sc, fs))
                    ls.gotoTarget = fs;
                fs.relatedLabeled = s;
            }
            result = s;
            return;
        }
        assert(fs._init is null);

        auto sym = new ScopeDsymbol();
        sym.parent = sc.scopesym;
        sym.endlinnum = fs.endloc.linnum;
        sc = sc.push(sym);
        sc.inLoop = true;

        if (fs.condition)
        {
            if (auto dotid = fs.condition.isDotIdExp())
                dotid.noderef = true;

            // check in syntax level
            fs.condition = checkAssignmentAsCondition(fs.condition, sc);

            fs.condition = fs.condition.expressionSemantic(sc);
            fs.condition = resolveProperties(sc, fs.condition);
            if (checkNonAssignmentArrayOp(fs.condition))
                fs.condition = ErrorExp.get();
            fs.condition = fs.condition.optimize(WANTvalue);
            fs.condition = checkGC(sc, fs.condition);

            fs.condition = fs.condition.toBoolean(sc);
        }
        if (fs.increment)
        {
            CommaExp.allow(fs.increment);
            fs.increment = fs.increment.expressionSemantic(sc);
            fs.increment = resolveProperties(sc, fs.increment);
            // @@@DEPRECATED_2.112@@@
            // remove gagging and deprecation() to turn deprecation into an error when
            // deprecation cycle is over
            const olderrors = global.startGagging();
            discardValue(fs.increment);
            if (global.endGagging(olderrors))
                deprecation(fs.increment.loc, "`%s` has no effect", fs.increment.toChars());
            if (checkNonAssignmentArrayOp(fs.increment))
                fs.increment = ErrorExp.get();
            fs.increment = fs.increment.optimize(WANTvalue);
            fs.increment = checkGC(sc, fs.increment);
        }

        sc.sbreak = fs;
        sc.scontinue = fs;
        if (fs._body)
            fs._body = fs._body.semanticNoScope(sc);

        sc.pop();

        if (fs.condition && fs.condition.op == EXP.error ||
            fs.increment && fs.increment.op == EXP.error ||
            fs._body && fs._body.isErrorStatement())
            return setError();
        result = fs;
    }

    void visitForeach(ForeachStatement fs)
    {
        /* https://dlang.org/spec/statement.html#foreach-statement
         */

        //printf("ForeachStatement::semantic() %p\n", fs);

        /******
         * Issue error if any of the ForeachTypes were not supplied and could not be inferred.
         * Returns:
         *      true if error issued
         */
        static bool checkForArgTypes(ForeachStatement fs)
        {
            bool result = false;
            foreach (p; *fs.parameters)
            {
                if (!p.type)
                {
                    error(fs.loc, "cannot infer type for `foreach` variable `%s`, perhaps set it explicitly", p.ident.toChars());
                    p.type = Type.terror;
                    result = true;
                }
            }
            return result;
        }

        const loc = fs.loc;
        const dim = fs.parameters.length;

        fs.func = sc.func;
        if (fs.func.fes)
            fs.func = fs.func.fes.func;

        VarDeclaration vinit = null;
        fs.aggr = fs.aggr.expressionSemantic(sc);
        fs.aggr = resolveProperties(sc, fs.aggr);
        fs.aggr = fs.aggr.optimize(WANTvalue);
        if (fs.aggr.op == EXP.error)
            return setError();
        Expression oaggr = fs.aggr;     // remember original for error messages
        if (fs.aggr.type && fs.aggr.type.toBasetype().isTypeStruct() &&
            fs.aggr.type.toBasetype().isTypeStruct().sym.dtor &&
            !fs.aggr.isTypeExp() && !fs.aggr.isLvalue())
        {
            // https://issues.dlang.org/show_bug.cgi?id=14653
            // Extend the life of rvalue aggregate till the end of foreach.
            vinit = copyToTemp(STC.rvalue, "__aggr", fs.aggr);
            vinit.endlinnum = fs.endloc.linnum;
            vinit.dsymbolSemantic(sc);
            fs.aggr = new VarExp(fs.aggr.loc, vinit);
        }

        /* If aggregate is a vector type, add the .array to make it a static array
         */
        if (fs.aggr.type)
            if (auto tv = fs.aggr.type.toBasetype().isTypeVector())
            {
                auto vae = new VectorArrayExp(fs.aggr.loc, fs.aggr);
                vae.type = tv.basetype;
                fs.aggr = vae;
            }

        Dsymbol sapply = null;                  // the inferred opApply() or front() function
        if (!inferForeachAggregate(sc, fs.op == TOK.foreach_, fs.aggr, sapply))
        {
            assert(oaggr.type);

            error(fs.loc, "invalid `%s` aggregate `%s` of type `%s`",
                Token.toChars(fs.op), oaggr.toChars(), oaggr.type.toPrettyChars());

            if (auto ad = isAggregate(fs.aggr.type))
            {
                if (fs.op == TOK.foreach_reverse_)
                {
                    fs.loc.errorSupplemental("`foreach_reverse` works with bidirectional ranges"~
                        " (implementing `back` and `popBack`), aggregates implementing" ~
                        " `opApplyReverse`, or the result of an aggregate's `.tupleof` property");
                    fs.loc.errorSupplemental("https://dlang.org/phobos/std_range_primitives.html#isBidirectionalRange");
                }
                else
                {
                    fs.loc.errorSupplemental("`foreach` works with input ranges"~
                        " (implementing `front` and `popFront`), aggregates implementing" ~
                        " `opApply`, or the result of an aggregate's `.tupleof` property");
                    fs.loc.errorSupplemental("https://dlang.org/phobos/std_range_primitives.html#isInputRange");
                }
            }

            return setError();
        }

        Dsymbol sapplyOld = sapply; // 'sapply' will be NULL if and after 'inferApplyArgTypes' errors

        /* Check for inference errors and apply mutability checks inline */
        if (!inferApplyArgTypes(fs, sc, sapply))
        {
            bool foundMismatch = false;
            size_t foreachParamCount = 0;
            if (sapplyOld)
            {
                if (FuncDeclaration fd = sapplyOld.isFuncDeclaration())
                {
                    auto fparameters = fd.getParameterList();

                    if (fparameters.length == 1)
                    {
                        // first param should be the callback function
                        Parameter fparam = fparameters[0];
                        if ((fparam.type.ty == Tpointer ||
                             fparam.type.ty == Tdelegate) &&
                            fparam.type.nextOf().isTypeFunction())
                        {
                            auto tf = fparam.type.nextOf().isTypeFunction();
                            foreachParamCount = tf.parameterList.length;
                            foundMismatch = true;

                            // Mutability check
                            if (fs.aggr && fs.aggr.type && fd.type && fs.aggr.type.isConst() && !fd.type.isConst())
                            {
                                // First error: The call site
                                error(fs.loc, "mutable method `%s.%s` is not callable using a `const` object",
                                    fd.parent ? fd.parent.toPrettyChars() : "unknown", fd.toChars());

                                // Second error: Suggest how to fix
                                errorSupplemental(fd.loc, "Consider adding `const` or `inout` here");

                                return setError();
                            }
                        }
                    }
                }
            }

            //printf("dim = %d, parameters.length = %d\n", dim, parameters.length);
            if (foundMismatch && dim != foreachParamCount)
            {
                const(char)* plural = foreachParamCount > 1 ? "s" : "";
                error(fs.loc, "cannot infer argument types, expected %llu argument%s, not %llu",
                    cast(ulong) foreachParamCount, plural, cast(ulong) dim);
            }
            else
                error(fs.loc, "cannot uniquely infer `foreach` argument types");

            return setError();
        }

        // If inference succeeds, proceed with post-checks
        if (sapply && sapply.isFuncDeclaration())
        {
            FuncDeclaration fd = sapply.isFuncDeclaration();

            if (fs.aggr && fs.aggr.type && fd.type && fs.aggr.type.isConst() && !fd.type.isConst())
            {
                // First error: The call site
                error(fs.loc, "mutable method `%s.%s` is not callable using a `const` object",
                    fd.parent ? fd.parent.toPrettyChars() : "unknown", fd.toChars());

                // Second error: Suggest how to fix
                errorSupplemental(fd.loc, "Consider adding `const` or `inout` here");

                return setError();
            }
        }

        Type tab = fs.aggr.type.toBasetype();

        if (tab.ty == Ttuple) // don't generate new scope for tuple loops
        {
            Statement s = makeTupleForeach(sc, false, false, fs, null, false).statement;
            if (vinit)
                s = new CompoundStatement(loc, new ExpStatement(loc, vinit), s);
            result = s.statementSemantic(sc);
            return;
        }

        auto sym = new ScopeDsymbol();
        sym.parent = sc.scopesym;
        sym.endlinnum = fs.endloc.linnum;
        auto sc2 = sc.push(sym);
        sc2.inLoop = true;

        foreach (Parameter p; *fs.parameters)
        {
            if (p.storageClass & STC.manifest)
            {
                error(fs.loc, "cannot declare `enum` loop variables for non-unrolled foreach");
            }
            if (p.storageClass & STC.alias_)
            {
                error(fs.loc, "cannot declare `alias` loop variables for non-unrolled foreach");
            }
        }

        void retError()
        {
            sc2.pop();
            result = new ErrorStatement();
        }

        void rangeError()
        {
            error(fs.loc, "cannot infer argument types");
            return retError();
        }

        void retStmt(Statement s)
        {
            if (!s)
                return retError();
            s = s.statementSemantic(sc2);
            sc2.pop();
            result = s;
        }

        Type tn = null;
        Type tnv = null;
        Statement apply()
        {
            if (checkForArgTypes(fs))
                return null;

            TypeFunction tfld = null;
            if (sapply)
            {
                if (auto fdapply = sapply.isFuncDeclaration())
                {
                    assert(fdapply.type && fdapply.type.isTypeFunction());
                    tfld = fdapply.type.typeSemantic(loc, sc2).isTypeFunction();
                    goto Lget;
                }
                else if (tab.isTypeDelegate())
                {
                    tfld = tab.nextOf().isTypeFunction();
                Lget:
                    //printf("tfld = %s\n", tfld.toChars());
                    if (tfld.parameterList.parameters.length == 1)
                    {
                        Parameter p = tfld.parameterList[0];
                        if (p.type && p.type.isTypeDelegate())
                        {
                            auto t = p.type.typeSemantic(loc, sc2);
                            assert(t.ty == Tdelegate);
                            tfld = t.nextOf().isTypeFunction();
                        }
                    }
                }
            }

            FuncExp flde = foreachBodyToFunction(sc2, fs, tfld);
            if (!flde)
                return null;

            // Resolve any forward referenced goto's
            foreach (ScopeStatement ss; *fs.gotos)
            {
                GotoStatement gs = ss.statement.isGotoStatement();
                if (!gs.label.statement)
                {
                    // 'Promote' it to this scope, and replace with a return
                    fs.cases.push(gs);
                    ss.statement = new ReturnStatement(Loc.initial, new IntegerExp(fs.cases.length + 1));
                }
            }

            Expression e = null;
            if (vinit)
            {
                e = new DeclarationExp(loc, vinit);
                e = e.expressionSemantic(sc2);
                if (e.op == EXP.error)
                    return null;
            }

            Expression ec;
            switch (tab.ty)
            {
                case Tarray:
                case Tsarray:   ec = applyArray     (fs, flde, tab, sc2, tn, tnv); break;
                case Tdelegate: ec = applyDelegate  (fs, flde, tab, sc2);          break;
                case Taarray:   ec = applyAssocArray(fs, flde, tab);               break;
                default:        ec = applyOpApply   (fs, flde, tab, sc2, sapply);  break;
            }
            if (!ec)
                return null;

            e = Expression.combine(e, ec);
            return loopReturn(e, fs.cases, loc);
        }

        switch (tab.ty)
        {
        case Tarray:
        case Tsarray:
            {
                if (checkForArgTypes(fs))
                    return retError();

                if (dim < 1 || dim > 2)
                {
                    error(fs.loc, "only one or two arguments for array `foreach`");
                    return retError();
                }

                // Finish semantic on all foreach parameter types.
                foreach (i; 0 .. dim)
                {
                    Parameter p = (*fs.parameters)[i];
                    p.type = p.type.typeSemantic(loc, sc2);
                    p.type = p.type.addStorageClass(p.storageClass);
                }

                tn = tab.nextOf().toBasetype();

                if (dim == 2)
                {
                    Type tindex = (*fs.parameters)[0].type;
                    if (!tindex.isIntegral())
                    {
                        error(fs.loc, "foreach: key cannot be of non-integral type `%s`", tindex.toChars());
                        return retError();
                    }
                    /* What cases to deprecate implicit conversions for:
                     *  1. foreach aggregate is a dynamic array
                     *  2. foreach body is lowered to _aApply (see special case below).
                     */
                    Type tv = (*fs.parameters)[1].type.toBasetype();
                    if ((tab.isTypeDArray() ||
                         (tn.ty != tv.ty && tn.ty.isSomeChar && tv.ty.isSomeChar)) &&
                        !Type.tsize_t.implicitConvTo(tindex))
                    {
                        bool err = true;
                        if (tab.isTypeDArray())
                        {
                            // check if overflow is possible
                            const maxLen = IntRange.fromType(tindex).imax.value + 1;
                            if (auto ale = fs.aggr.isArrayLiteralExp())
                                err = ale.elements.length > maxLen;
                            else if (auto se = fs.aggr.isSliceExp())
                                err = !(se.upr && se.upr.isConst() && se.upr.toInteger() <= maxLen);
                        }
                        if (err)
                            deprecation(fs.loc, "foreach: loop index implicitly converted from `size_t` to `%s`",
                                       tindex.toChars());
                    }
                }

                /* Look for special case of parsing char types out of char type
                 * array.
                 */
                if (tn.ty.isSomeChar)
                {
                    int i = (dim == 1) ? 0 : 1; // index of value
                    Parameter p = (*fs.parameters)[i];
                    tnv = p.type.toBasetype();
                    if (tnv.ty != tn.ty && tnv.ty.isSomeChar)
                    {
                        if (p.storageClass & STC.ref_)
                        {
                            error(fs.loc, "`foreach`: value of UTF conversion cannot be `ref`");
                            return retError();
                        }
                        if (dim == 2)
                        {
                            p = (*fs.parameters)[0];
                            if (p.storageClass & STC.ref_)
                            {
                                error(fs.loc, "`foreach`: key cannot be `ref`");
                                return retError();
                            }
                        }
                        return retStmt(apply());
                    }
                }

                // Declare the key
                if (dim == 2)
                {
                    Parameter p = (*fs.parameters)[0];
                    fs.key = new VarDeclaration(loc, p.type.mutableOf(), Identifier.generateId("__key"), null);
                    fs.key.storage_class |= STC.temp | STC.foreach_;
                    if (fs.key.isReference())
                        fs.key.storage_class |= STC.nodtor;

                    if (p.storageClass & STC.ref_)
                    {
                        if (fs.key.type.constConv(p.type) == MATCH.nomatch)
                        {
                            error(fs.loc, "key type mismatch, `%s` to `ref %s`",
                                     fs.key.type.toChars(), p.type.toChars());
                            return retError();
                        }
                    }
                    if (auto ta = tab.isTypeSArray())
                    {
                        IntRange dimrange = getIntRange(ta.dim);
                        // https://issues.dlang.org/show_bug.cgi?id=12504
                        dimrange.imax = SignExtendedNumber(dimrange.imax.value-1);
                        if (!IntRange.fromType(fs.key.type).contains(dimrange))
                        {
                            error(fs.loc, "index type `%s` cannot cover index range 0..%llu",
                                     p.type.toChars(), ta.dim.toInteger());
                            return retError();
                        }
                        fs.key.range = new IntRange(SignExtendedNumber(0), dimrange.imax);
                    }
                }
                // Now declare the value
                {
                    Parameter p = (*fs.parameters)[dim - 1];
                    fs.value = new VarDeclaration(loc, p.type, p.ident, null);
                    fs.value.storage_class |= STC.foreach_;
                    fs.value.storage_class |= p.storageClass & (STC.scope_ | STC.IOR | STC.TYPECTOR);
                    if (fs.value.isReference())
                    {
                        fs.value.storage_class |= STC.nodtor;

                        if (fs.aggr.checkModifiable(sc2, ModifyFlags.noError) == Modifiable.initialization)
                            fs.value.setInCtorOnly = true;

                        Type t = tab.nextOf();
                        if (t.constConv(p.type) == MATCH.nomatch)
                        {
                            error(fs.loc, "argument type mismatch, `%s` to `ref %s`",
                                     t.toChars(), p.type.toChars());
                            return retError();
                        }
                    }
                }

                /* Convert to a ForStatement
                 *   foreach (key, value; a) body =>
                 *   for (T[] tmp = a[], size_t key; key < tmp.length; ++key)
                 *   { T value = tmp[k]; body }
                 *
                 *   foreach_reverse (key, value; a) body =>
                 *   for (T[] tmp = a[], size_t key = tmp.length; key--; )
                 *   { T value = tmp[k]; body }
                 */
                auto id = Identifier.generateId("__r");
                auto ie = new ExpInitializer(loc, new SliceExp(loc, fs.aggr, null, null));
                const valueIsRef = (*fs.parameters)[$ - 1].isReference();
                VarDeclaration tmp;
                if (fs.aggr.isArrayLiteralExp() && !valueIsRef)
                {
                    auto ale = fs.aggr.isArrayLiteralExp();
                    size_t edim = ale.elements ? ale.elements.length : 0;
                    auto telem = (*fs.parameters)[dim - 1].type;

                    // https://issues.dlang.org/show_bug.cgi?id=12936
                    // if telem has been specified explicitly,
                    // converting array literal elements to telem might make it @nogc.
                    fs.aggr = fs.aggr.implicitCastTo(sc, telem.sarrayOf(edim));
                    if (fs.aggr.op == EXP.error)
                        return retError();

                    // for (T[edim] tmp = a, ...)
                    tmp = new VarDeclaration(loc, fs.aggr.type, id, ie);
                }
                else
                {
                    tmp = new VarDeclaration(loc, tab.nextOf().arrayOf(), id, ie);
                    if (!valueIsRef)
                        tmp.storage_class |= STC.scope_;
                }
                tmp.storage_class |= STC.temp;

                Expression tmp_length = new DotIdExp(loc, new VarExp(loc, tmp), Id.length);

                if (!fs.key)
                {
                    Identifier idkey = Identifier.generateId("__key");
                    fs.key = new VarDeclaration(loc, Type.tsize_t, idkey, null);
                    fs.key.storage_class |= STC.temp;
                }
                else if (fs.key.type.ty != Type.tsize_t.ty)
                {
                    tmp_length = new CastExp(loc, tmp_length, fs.key.type);
                }
                if (fs.op == TOK.foreach_reverse_)
                    fs.key._init = new ExpInitializer(loc, tmp_length);
                else
                    fs.key._init = new ExpInitializer(loc, new IntegerExp(loc, 0, fs.key.type));

                auto cs = new Statements();
                if (vinit)
                    cs.push(new ExpStatement(loc, vinit));
                cs.push(new ExpStatement(loc, tmp));
                cs.push(new ExpStatement(loc, fs.key));
                Statement forinit = new CompoundDeclarationStatement(loc, cs);

                Expression cond;
                if (fs.op == TOK.foreach_reverse_)
                {
                    // key--
                    cond = new PostExp(EXP.minusMinus, loc, new VarExp(loc, fs.key));
                }
                else
                {
                    // key < tmp.length
                    cond = new CmpExp(EXP.lessThan, loc, new VarExp(loc, fs.key), tmp_length);
                }

                Expression increment = null;
                if (fs.op == TOK.foreach_)
                {
                    // key += 1
                    increment = new AddAssignExp(loc, new VarExp(loc, fs.key), new IntegerExp(loc, 1, fs.key.type));
                }

                // T value = tmp[key];
                IndexExp indexExp = new IndexExp(loc, new VarExp(loc, tmp), new VarExp(loc, fs.key));
                indexExp.indexIsInBounds = true; // disabling bounds checking in foreach statements.
                fs.value._init = new ExpInitializer(loc, indexExp);
                Statement ds = new ExpStatement(loc, fs.value);

                if (dim == 2)
                {
                    Parameter p = (*fs.parameters)[0];
                    if ((p.storageClass & STC.ref_) && p.type.equals(fs.key.type))
                    {
                        fs.key.range = null;
                        auto v = new AliasDeclaration(loc, p.ident, fs.key);
                        fs._body = new CompoundStatement(loc, new ExpStatement(loc, v), fs._body);
                    }
                    else
                    {
                        auto ei = new ExpInitializer(loc, new IdentifierExp(loc, fs.key.ident));
                        auto v = new VarDeclaration(loc, p.type, p.ident, ei);
                        v.storage_class |= STC.foreach_ | (p.storageClass & STC.ref_);
                        fs._body = new CompoundStatement(loc, new ExpStatement(loc, v), fs._body);
                        if (fs.key.range && !p.type.isMutable())
                        {
                            /* Limit the range of the key to the specified range
                             */
                            v.range = new IntRange(fs.key.range.imin, fs.key.range.imax - SignExtendedNumber(1));
                        }
                    }
                }
                fs._body = new CompoundStatement(loc, ds, fs._body);

                Statement s = new ForStatement(loc, forinit, cond, increment, fs._body, fs.endloc);
                if (auto ls = checkLabeledLoop(sc, fs))   // https://issues.dlang.org/show_bug.cgi?id=15450
                                                          // don't use sc2
                    ls.gotoTarget = s;
                return retStmt(s);
            }
        case Taarray:
            if (fs.op == TOK.foreach_reverse_)
                error(fs.loc, "cannot use `foreach_reverse` with an associative array");
            if (checkForArgTypes(fs))
                return retError();

            if (dim < 1 || dim > 2)
            {
                error(fs.loc, "only one or two arguments for associative array `foreach`");
                return retError();
            }
            return retStmt(apply());

        case Tclass:
        case Tstruct:
            /* Prefer using opApply, if it exists
             */
            if (sapply)
                return retStmt(apply());
            {
                /* Look for range iteration, i.e. the properties
                 * .empty, .popFront, .popBack, .front and .back
                 *    foreach (e; aggr) { ... }
                 * translates to:
                 *    for (auto __r = aggr[]; !__r.empty; __r.popFront()) {
                 *        auto e = __r.front;
                 *        ...
                 *    }
                 */
                auto ad = (tab.ty == Tclass) ?
                    cast(AggregateDeclaration)tab.isTypeClass().sym :
                    cast(AggregateDeclaration)tab.isTypeStruct().sym;
                Identifier idfront;
                Identifier idpopFront;
                if (fs.op == TOK.foreach_)
                {
                    idfront = Id.Ffront;
                    idpopFront = Id.FpopFront;
                }
                else
                {
                    idfront = Id.Fback;
                    idpopFront = Id.FpopBack;
                }
                auto sfront = ad.search(Loc.initial, idfront);
                if (!sfront)
                    return retStmt(apply());

                /* Generate a temporary __r and initialize it with the aggregate.
                 */
                VarDeclaration r;
                Statement _init;
                if (vinit && fs.aggr.isVarExp() && fs.aggr.isVarExp().var == vinit)
                {
                    r = vinit;
                    _init = new ExpStatement(loc, vinit);
                }
                else
                {
                    r = copyToTemp(0, "__r", fs.aggr);
                    r.dsymbolSemantic(sc);
                    _init = new ExpStatement(loc, r);
                    if (vinit)
                        _init = new CompoundStatement(loc, new ExpStatement(loc, vinit), _init);
                }

                // !__r.empty
                Expression e = new VarExp(loc, r);
                e = new DotIdExp(loc, e, Id.Fempty);
                Expression condition = new NotExp(loc, e);

                // __r.idpopFront()
                e = new VarExp(loc, r);
                Expression increment = new CallExp(loc, new DotIdExp(loc, e, idpopFront));

                /* Declaration statement for e:
                 *    auto e = __r.idfront;
                 */
                e = new VarExp(loc, r);
                Expression einit = new DotIdExp(loc, e, idfront);
                Statement makeargs, forbody;
                bool ignoreRef = false; // If a range returns a non-ref front we ignore ref on foreach

                Type tfront;
                if (auto fd = sfront.isFuncDeclaration())
                {
                    if (!functionSemantic(fd))
                        return rangeError();
                    tfront = fd.type;
                }
                else if (auto td = sfront.isTemplateDeclaration())
                {
                    if (auto f = resolveFuncCall(loc, sc, td, null, tab, ArgumentList(), FuncResolveFlag.quiet))
                        tfront = f.type;
                }
                else if (auto d = sfront.toAlias().isDeclaration())
                {
                    tfront = d.type;
                }
                if (!tfront || tfront.ty == Terror)
                    return rangeError();
                if (auto ftt = tfront.toBasetype().isTypeFunction())
                {
                    tfront = tfront.toBasetype().nextOf();
                    if (!ftt.isRef)
                    {
                        // .front() does not return a ref. We ignore ref on foreach arg.
                        // see https://issues.dlang.org/show_bug.cgi?id=11934
                        if (tfront.needsDestruction()) ignoreRef = true;
                    }
                }
                if (tfront.ty == Tvoid)
                {
                    error(fs.loc, "`%s.front` is `void` and has no value", oaggr.toChars());
                    return retError();
                }

                if (dim == 1)
                {
                    auto p = (*fs.parameters)[0];
                    auto ve = new VarDeclaration(loc, p.type, p.ident, new ExpInitializer(loc, einit));
                    ve.storage_class |= STC.foreach_;
                    ve.storage_class |= p.storageClass & (STC.scope_ | STC.IOR | STC.TYPECTOR);

                    if (ignoreRef)
                        ve.storage_class &= ~STC.ref_;

                    makeargs = new ExpStatement(loc, ve);
                }
                else
                {
                    auto vd = copyToTemp(STC.ref_, "__front", einit);
                    vd.dsymbolSemantic(sc);
                    makeargs = new ExpStatement(loc, vd);

                    // Resolve inout qualifier of front type
                    tfront = tfront.substWildTo(tab.mod);

                    Expression ve = new VarExp(loc, vd);
                    ve.type = tfront;

                    auto exps = new Expressions();
                    exps.push(ve);
                    int pos = 0;
                    while (exps.length < dim)
                    {
                        pos = expandAliasThisTuples(exps, pos);
                        if (pos == -1)
                            break;
                    }
                    if (exps.length != dim)
                    {
                        const(char)* plural = exps.length > 1 ? "s" : "";
                        error(fs.loc, "cannot infer argument types, expected %llu argument%s, not %llu",
                            cast(ulong) exps.length, plural, cast(ulong) dim);
                        return retError();
                    }

                    foreach (i; 0 .. dim)
                    {
                        auto p = (*fs.parameters)[i];
                        auto exp = (*exps)[i];
                        version (none)
                        {
                            printf("[%lu] p = %s %s, exp = %s %s\n", i,
                                p.type ? p.type.toChars() : "?", p.ident.toChars(),
                                exp.type.toChars(), exp.toChars());
                        }
                        if (!p.type)
                            p.type = exp.type;

                        auto sc = p.storageClass;
                        if (ignoreRef) sc &= ~STC.ref_;
                        p.type = p.type.addStorageClass(sc).typeSemantic(loc, sc2);
                        if (!exp.implicitConvTo(p.type))
                        {
                            error(fs.loc, "cannot implicitly convert tuple element of type `%s` to variable `%s` of type `%s`",
                                exp.type.toChars(), p.toChars(), p.type.toChars());
                            return retError();
                        }

                        auto var = new VarDeclaration(loc, p.type, p.ident, new ExpInitializer(loc, exp));
                        var.storage_class |= STC.ctfe | STC.ref_ | STC.foreach_;
                        makeargs = new CompoundStatement(loc, makeargs, new ExpStatement(loc, var));
                    }
                }

                forbody = new CompoundStatement(loc, makeargs, fs._body);

                Statement s = new ForStatement(loc, _init, condition, increment, forbody, fs.endloc);
                if (auto ls = checkLabeledLoop(sc, fs))
                    ls.gotoTarget = s;

                version (none)
                {
                    printf("init: %s\n", toChars(_init));
                    printf("condition: %s\n", condition.toChars());
                    printf("increment: %s\n", increment.toChars());
                    printf("body: %s\n", forbody.toChars());
                }
                return retStmt(s);
            }
        case Tdelegate:
            if (fs.op == TOK.foreach_reverse_)
                error(fs.loc, "cannot use `foreach_reverse` with a delegate");
            return retStmt(apply());
        case Terror:
            return retError();
        default:
            error(fs.loc, "`foreach`: `%s` is not an aggregate type", fs.aggr.type.toChars());
            return retError();
        }
    }

    void visitForeachRange(ForeachRangeStatement fs)
    {
        /* https://dlang.org/spec/statement.html#foreach-range-statement
         */

        //printf("ForeachRangeStatement::semantic() %p\n", fs);
        auto loc = fs.loc;
        fs.lwr = fs.lwr.expressionSemantic(sc);
        fs.lwr = resolveProperties(sc, fs.lwr);
        fs.lwr = fs.lwr.optimize(WANTvalue);
        if (!fs.lwr.type)
        {
            error(fs.loc, "invalid range lower bound `%s`", fs.lwr.toChars());
            return setError();
        }

        fs.upr = fs.upr.expressionSemantic(sc);
        fs.upr = resolveProperties(sc, fs.upr);
        fs.upr = fs.upr.optimize(WANTvalue);
        if (!fs.upr.type)
        {
            error(fs.loc, "invalid range upper bound `%s`", fs.upr.toChars());
            return setError();
        }

        if (fs.param.type)
        {
            fs.param.type = fs.param.type.typeSemantic(loc, sc);
            fs.param.type = fs.param.type.addStorageClass(fs.param.storageClass);
            fs.lwr = fs.lwr.implicitCastTo(sc, fs.param.type);

            if (fs.upr.implicitConvTo(fs.param.type) || (fs.param.storageClass & STC.ref_))
            {
                fs.upr = fs.upr.implicitCastTo(sc, fs.param.type);
            }
            else
            {
                // See if upr-1 fits in param.type
                Expression limit = new MinExp(loc, fs.upr, IntegerExp.literal!1);
                limit = limit.expressionSemantic(sc);
                limit = limit.optimize(WANTvalue);
                if (!limit.implicitConvTo(fs.param.type))
                {
                    fs.upr = fs.upr.implicitCastTo(sc, fs.param.type);
                }
            }
        }
        else
        {
            /* Must infer types from lwr and upr
             */
            Type tlwr = fs.lwr.type.toBasetype();
            if (tlwr.ty == Tstruct || tlwr.ty == Tclass)
            {
                /* Just picking the first really isn't good enough.
                 */
                fs.param.type = fs.lwr.type;
            }
            else if (fs.lwr.type == fs.upr.type)
            {
                /* Same logic as CondExp ?lwr:upr
                 */
                fs.param.type = fs.lwr.type;
            }
            else
            {
                scope AddExp ea = new AddExp(loc, fs.lwr, fs.upr);
                if (typeCombine(ea, sc))
                    return setError();
                fs.param.type = ea.type;
                fs.lwr = ea.e1;
                fs.upr = ea.e2;
            }
            fs.param.type = fs.param.type.addStorageClass(fs.param.storageClass);
        }
        if (fs.param.type.ty == Terror || fs.lwr.op == EXP.error || fs.upr.op == EXP.error)
        {
            return setError();
        }

        /* Convert to a for loop:
         *  foreach (key; lwr .. upr) =>
         *  for (auto key = lwr, auto tmp = upr; key < tmp; ++key)
         *
         *  foreach_reverse (key; lwr .. upr) =>
         *  for (auto tmp = lwr, auto key = upr; key-- > tmp;)
         */
        auto ie = new ExpInitializer(loc, (fs.op == TOK.foreach_) ? fs.lwr : fs.upr);
        fs.key = new VarDeclaration(loc, fs.upr.type.mutableOf(), Identifier.generateId("__key"), ie);
        fs.key.storage_class |= STC.temp;
        SignExtendedNumber lower = getIntRange(fs.lwr).imin;
        SignExtendedNumber upper = getIntRange(fs.upr).imax;
        if (lower <= upper)
        {
            fs.key.range = new IntRange(lower, upper);
        }

        Identifier id = Identifier.generateId("__limit");
        ie = new ExpInitializer(loc, (fs.op == TOK.foreach_) ? fs.upr : fs.lwr);
        auto tmp = new VarDeclaration(loc, fs.upr.type, id, ie);
        tmp.storage_class |= STC.temp;

        auto cs = new Statements();
        // Keep order of evaluation as lwr, then upr
        if (fs.op == TOK.foreach_)
        {
            cs.push(new ExpStatement(loc, fs.key));
            cs.push(new ExpStatement(loc, tmp));
        }
        else
        {
            cs.push(new ExpStatement(loc, tmp));
            cs.push(new ExpStatement(loc, fs.key));
        }
        Statement forinit = new CompoundDeclarationStatement(loc, cs);

        Expression cond;
        if (fs.op == TOK.foreach_reverse_)
        {
            cond = new PostExp(EXP.minusMinus, loc, new VarExp(loc, fs.key));
            if (fs.param.type.isScalar())
            {
                // key-- > tmp
                cond = new CmpExp(EXP.greaterThan, loc, cond, new VarExp(loc, tmp));
            }
            else
            {
                // key-- != tmp
                cond = new EqualExp(EXP.notEqual, loc, cond, new VarExp(loc, tmp));
            }
        }
        else
        {
            if (fs.param.type.isScalar())
            {
                // key < tmp
                cond = new CmpExp(EXP.lessThan, loc, new VarExp(loc, fs.key), new VarExp(loc, tmp));
            }
            else
            {
                // key != tmp
                cond = new EqualExp(EXP.notEqual, loc, new VarExp(loc, fs.key), new VarExp(loc, tmp));
            }
        }

        Expression increment = null;
        if (fs.op == TOK.foreach_)
        {
            // key += 1
            //increment = new AddAssignExp(loc, new VarExp(loc, fs.key), IntegerExp.literal!1);
            increment = new PreExp(EXP.prePlusPlus, loc, new VarExp(loc, fs.key));
        }
        if ((fs.param.storageClass & STC.ref_) && fs.param.type.equals(fs.key.type))
        {
            fs.key.range = null;
            auto v = new AliasDeclaration(loc, fs.param.ident, fs.key);
            fs._body = new CompoundStatement(loc, new ExpStatement(loc, v), fs._body);
        }
        else
        {
            ie = new ExpInitializer(loc, new CastExp(loc, new VarExp(loc, fs.key), fs.param.type));
            auto v = new VarDeclaration(loc, fs.param.type, fs.param.ident, ie);
            v.storage_class |= STC.temp | STC.foreach_ | (fs.param.storageClass & STC.ref_);
            fs._body = new CompoundStatement(loc, new ExpStatement(loc, v), fs._body);
            if (fs.key.range && !fs.param.type.isMutable())
            {
                /* Limit the range of the key to the specified range
                 */
                v.range = new IntRange(fs.key.range.imin, fs.key.range.imax - SignExtendedNumber(1));
            }
        }
        if (fs.param.storageClass & STC.ref_)
        {
            if (fs.key.type.constConv(fs.param.type) == MATCH.nomatch)
            {
                error(fs.loc, "argument type mismatch, `%s` to `ref %s`", fs.key.type.toChars(), fs.param.type.toChars());
                return setError();
            }
        }

        auto s = new ForStatement(loc, forinit, cond, increment, fs._body, fs.endloc);
        if (LabelStatement ls = checkLabeledLoop(sc, fs))
            ls.gotoTarget = s;
        result = s.statementSemantic(sc);
    }

    void visitIf(IfStatement ifs)
    {
        /* https://dlang.org/spec/statement.html#IfStatement
         */

        // check in syntax level
        ifs.condition = checkAssignmentAsCondition(ifs.condition, sc);

        auto sym = new ScopeDsymbol();
        sym.parent = sc.scopesym;
        sym.endlinnum = ifs.endloc.linnum;
        Scope* scd = sc.push(sym);
        if (ifs.param)
        {
            /* Declare param, which we will set to be the
             * result of condition.
             */
            auto ei = new ExpInitializer(ifs.loc, ifs.condition);
            ifs.match = new VarDeclaration(ifs.loc, ifs.param.type, ifs.param.ident, ei);
            ifs.match.parent = scd.func;
            ifs.match.storage_class |= ifs.param.storageClass;
            ifs.match.dsymbolSemantic(scd);

            auto de = new DeclarationExp(ifs.loc, ifs.match);
            auto ve = new VarExp(ifs.loc, ifs.match);
            ifs.condition = new CommaExp(ifs.loc, de, ve);
            ifs.condition = ifs.condition.expressionSemantic(scd);

            if (ifs.match.edtor)
            {
                Statement sdtor = new DtorExpStatement(ifs.loc, ifs.match.edtor, ifs.match);
                sdtor = new ScopeGuardStatement(ifs.loc, TOK.onScopeExit, sdtor);
                ifs.ifbody = new CompoundStatement(ifs.loc, sdtor, ifs.ifbody);
                ifs.match.storage_class |= STC.nodtor;

                // the destructor is always called
                // whether the 'ifbody' is executed or not
                Statement sdtor2 = new DtorExpStatement(ifs.loc, ifs.match.edtor, ifs.match);
                if (ifs.elsebody)
                    ifs.elsebody = new CompoundStatement(ifs.loc, sdtor2, ifs.elsebody);
                else
                    ifs.elsebody = sdtor2;
            }
        }
        else
        {
            if (auto dotid = ifs.condition.isDotIdExp())
                dotid.noderef = true;

            ifs.condition = ifs.condition.expressionSemantic(scd);
            ifs.condition = resolveProperties(scd, ifs.condition);
            ifs.condition = ifs.condition.addDtorHook(scd);
        }
        if (checkNonAssignmentArrayOp(ifs.condition))
            ifs.condition = ErrorExp.get();

        // Convert to boolean after declaring param so this works:
        //  if (S param = S()) {}
        // where S is a struct that defines opCast!bool.
        ifs.condition = ifs.condition.toBoolean(scd);

        // If we can short-circuit evaluate the if statement, don't do the
        // semantic analysis of the skipped code.
        // This feature allows a limited form of conditional compilation.
        ifs.condition = ifs.condition.optimize(WANTvalue);

        // checkGC after optimizing the condition so that
        // compile time constants are reduced.
        ifs.condition = checkGC(scd, ifs.condition);

        // Save 'root' of two branches (then and else) at the point where it forks
        CtorFlow ctorflow_root = scd.ctorflow.clone();

        /* Rewrite `if (!__ctfe) A else B` as `if (__ctfe) B else A`
         */
        NotExp notExp;
        if (ifs.elsebody &&
            (notExp = ifs.condition.isNotExp()) !is null &&
            notExp.e1.isVarExp() &&
            notExp.e1.isVarExp().var.ident == Id.ctfe)
        {
            ifs.condition = notExp.e1;
            auto sbody = ifs.ifbody;
            ifs.ifbody = ifs.elsebody;
            ifs.elsebody = sbody;
        }

        /* Detect `if (__ctfe)`
         */
        if (ifs.isIfCtfeBlock())
        {
            Scope* scd2 = scd.push();
            scd2.ctfeBlock = true;
            ifs.ifbody = ifs.ifbody.semanticNoScope(scd2);
            scd2.pop();
        }
        else
            ifs.ifbody = ifs.ifbody.semanticNoScope(scd);
        scd.pop();

        CtorFlow ctorflow_then = sc.ctorflow;   // move flow results
        sc.ctorflow = ctorflow_root;            // reset flow analysis back to root
        if (ifs.elsebody)
            ifs.elsebody = ifs.elsebody.semanticScope(sc, null, null, null);

        // Merge 'then' results into 'else' results
        sc.merge(ifs.loc, ctorflow_then);

        ctorflow_then.freeFieldinit();          // free extra copy of the data

        if (ifs.condition.op == EXP.error ||
            (ifs.ifbody && ifs.ifbody.isErrorStatement()) ||
            (ifs.elsebody && ifs.elsebody.isErrorStatement()))
        {
            return setError();
        }
        result = ifs;
    }

    void visitConditional(ConditionalStatement cs)
    {
        //printf("ConditionalStatement::semantic()\n");

        // If we can short-circuit evaluate the if statement, don't do the
        // semantic analysis of the skipped code.
        // This feature allows a limited form of conditional compilation.
        if (cs.condition.include(sc))
        {
            if (DebugCondition dc = cs.condition.isDebugCondition())
            {
                sc = sc.push();
                sc.debug_ = true;
                cs.ifbody = cs.ifbody.statementSemantic(sc);
                sc.pop();
            }
            else
                cs.ifbody = cs.ifbody.statementSemantic(sc);
            result = cs.ifbody;
        }
        else
        {
            if (cs.elsebody)
                cs.elsebody = cs.elsebody.statementSemantic(sc);
            result = cs.elsebody;
        }
    }

    void visitPragma(PragmaStatement ps)
    {
        /* https://dlang.org/spec/statement.html#pragma-statement
         */
        import dmd.pragmasem : pragmaStmtSemantic;
        if (!pragmaStmtSemantic(ps, sc))
            return setError();

        result = ps._body;
    }

    void visitStaticAssert(StaticAssertStatement s)
    {
        s.sa.semantic2(sc);
        if (s.sa.errors)
            return setError();
    }

    void visitSwitch(SwitchStatement ss)
    {
        /* https://dlang.org/spec/statement.html#switch-statement
         */

        //printf("SwitchStatement::semantic(%p)\n", ss);
        ss.tryBody = sc.tryBody;
        ss.tf = sc.tf;
        if (ss.cases)
        {
            result = ss; // already run
            return;
        }

        if (ss.param)
        {
            /**
             * If the switch statement is of form `switch(auto a = exp) { body }`,
             * rewrite to the following inside it's own scope:
             *
             * auto a = exp
             * switch(a)
             *     { body }
             */
            auto statements = new Statements();
            auto vardecl = new VarDeclaration(ss.param.loc,
                ss.param.type,
                ss.param.ident,
                new ExpInitializer(ss.condition.loc, ss.condition),
                ss.param.storageClass);

            statements.push(new ExpStatement(ss.param.loc, vardecl));

            ss.condition = new VarExp(ss.param.loc, vardecl, false);
            ss.param = null;

            statements.push(ss);

            Statement s = new CompoundStatement(ss.loc, statements);
            s = new ScopeStatement(ss.loc, s, ss.endloc);
            s = s.statementSemantic(sc);
            result = s;
            return;
        }

        bool conditionError = false;
        ss.condition = ss.condition.expressionSemantic(sc);
        ss.condition = resolveProperties(sc, ss.condition);

        Type att = null;
        TypeEnum te = null;
        while (!ss.condition.isErrorExp())
        {
            // preserve enum type for final switches
            if (auto tenum = ss.condition.type.isTypeEnum())
                te = tenum;
            if (ss.condition.type.isString())
            {
                // If it's not an array, cast it to one
                if (ss.condition.type.ty != Tarray)
                {
                    ss.condition = ss.condition.implicitCastTo(sc, ss.condition.type.nextOf().arrayOf());
                }
                ss.condition.type = ss.condition.type.constOf();
                break;
            }
            ss.condition = integralPromotions(ss.condition, sc);
            if (!ss.condition.isErrorExp() && ss.condition.type.isIntegral())
                break;

            auto ad = isAggregate(ss.condition.type);
            if (ad && ad.aliasthis && !isRecursiveAliasThis(att, ss.condition.type))
            {
                if (auto e = resolveAliasThis(sc, ss.condition, true))
                {
                    ss.condition = e;
                    continue;
                }
            }

            if (!ss.condition.isErrorExp())
            {
                error(ss.loc, "`%s` must be of integral or string type, it is a `%s`",
                    ss.condition.toChars(), ss.condition.type.toChars());
                conditionError = true;
                break;
            }
        }
        if (checkNonAssignmentArrayOp(ss.condition))
            ss.condition = ErrorExp.get();
        ss.condition = ss.condition.optimize(WANTvalue);
        ss.condition = checkGC(sc, ss.condition);
        if (ss.condition.op == EXP.error)
            conditionError = true;

        bool needswitcherror = false;

        ss.lastVar = sc.lastVar;

        sc = sc.push();
        sc.sbreak = ss;
        sc.sw = ss;

        ss.cases = new CaseStatements();
        const inLoopSave = sc.inLoop;
        sc.inLoop = true;        // BUG: should use Scope::mergeCallSuper() for each case instead
        ss._body = ss._body.statementSemantic(sc);
        sc.inLoop = inLoopSave;

        if (conditionError || (ss._body && ss._body.isErrorStatement()))
        {
            sc.pop();
            return setError();
        }

        // Resolve any goto case's with exp
      Lgotocase:
        foreach (gcs; ss.gotoCases)
        {
            if (!gcs.exp)
            {
                error(gcs.loc, "no `case` statement following `goto case;`");
                sc.pop();
                return setError();
            }

            for (Scope* scx = sc; scx; scx = scx.enclosing)
            {
                if (!scx.sw)
                    continue;
                foreach (cs; *scx.sw.cases)
                {
                    if (cs.exp.equals(gcs.exp))
                    {
                        gcs.cs = cs;
                        continue Lgotocase;
                    }
                }
            }
            error(gcs.loc, "`case %s` not found", gcs.exp.toChars());
            sc.pop();
            return setError();
        }

        if (ss.isFinal)
        {
            Type t = ss.condition.type;
            Dsymbol ds;
            EnumDeclaration ed = null;
            if (t && ((ds = t.toDsymbol(sc)) !is null))
                ed = ds.isEnumDeclaration(); // typedef'ed enum
            if (!ed && te && ((ds = te.toDsymbol(sc)) !is null))
                ed = ds.isEnumDeclaration();
            if (ed && ss.cases.length < ed.members.length)
            {
                int missingMembers = 0;
                const maxShown = global.params.v.errorSupplementCount();
            Lmembers:
                foreach (es; *ed.members)
                {
                    EnumMember em = es.isEnumMember();
                    if (em)
                    {
                        foreach (cs; *ss.cases)
                        {
                            if (cs.exp.equals(em.value) || (!cs.exp.type.isString() &&
                                !em.value.type.isString() && cs.exp.toInteger() == em.value.toInteger()))
                                continue Lmembers;
                        }
                        if (missingMembers == 0)
                            error(ss.loc, "missing cases for `enum` members in `final switch`:");

                        if (missingMembers < maxShown)
                            errorSupplemental(ss.loc, "`%s`", em.toChars());
                        missingMembers++;
                    }
                }
                if (missingMembers > 0)
                {
                    if (missingMembers > maxShown)
                        errorSupplemental(ss.loc, "... (%d more, -v to show) ...", missingMembers - maxShown);
                    sc.pop();
                    return setError();
                }
            }
            else
                needswitcherror = true;
        }

        ss.hasDefault = sc.sw.sdefault ||
            !(!ss.isFinal || needswitcherror || global.params.useAssert == CHECKENABLE.on || sc.func.isSafe || sc.func.isSaferD);
        if (!ss.hasDefault)
        {
            if (!ss.isFinal && (!ss._body || !ss._body.isErrorStatement()) && !sc.inCfile)
                error(ss.loc, "`switch` statement without a `default`; use `final switch` or add `default: assert(0);` or add `default: break;`");

            // Generate runtime error if the default is hit
            auto a = new Statements();
            CompoundStatement cs;
            Statement s;

            if (sc.inCfile)
            {
                s = new BreakStatement(ss.loc, null);   // default for C is `default: break;`
            }
            else if (!sc.needsCodegen())
            {
                // something for the interpreter to deal with
                s = new ExpStatement(ss.loc, new AssertExp(ss.loc, IntegerExp.literal!0));
            }
            else if (global.params.useSwitchError == CHECKENABLE.on &&
                global.params.checkAction != CHECKACTION.halt)
            {
                if (global.params.checkAction == CHECKACTION.C)
                {
                    /* Rewrite as an assert(0) and let e2ir generate
                     * the call to the C assert failure function
                     */
                    s = new ExpStatement(ss.loc, new AssertExp(ss.loc, IntegerExp.literal!0));
                }
                else
                {
                    if (!verifyHookExist(ss.loc, *sc, Id.__switch_error, "generating assert messages"))
                        return setError();

                    Expression sl = new IdentifierExp(ss.loc, Id.empty);
                    sl = new DotIdExp(ss.loc, sl, Id.object);
                    sl = new DotIdExp(ss.loc, sl, Id.__switch_error);

                    Expressions* args = new Expressions(2);
                    (*args)[0] = new StringExp(ss.loc, ss.loc.filename.toDString());
                    (*args)[1] = new IntegerExp(ss.loc.linnum);

                    sl = new CallExp(ss.loc, sl, args);
                    sl = sl.expressionSemantic(sc);

                    s = new SwitchErrorStatement(ss.loc, sl);
                }
            }
            else
                s = new ExpStatement(ss.loc, new HaltExp(ss.loc));

            a.reserve(2);
            sc.sw.sdefault = new DefaultStatement(ss.loc, s);
            a.push(ss._body);
            if (ss._body.blockExit(sc.func, null) & BE.fallthru)
                a.push(new BreakStatement(Loc.initial, null));
            a.push(sc.sw.sdefault);
            cs = new CompoundStatement(ss.loc, a);
            ss._body = cs;
        }

        if (!sc.inCfile && ss.checkLabel())
        {
            sc.pop();
            return setError();
        }


        if (!(ss.condition.type.isString() && sc.needsCodegen()))
        {
            sc.pop();
            result = ss;
            return;
        }

        // Transform a switch with string labels into a switch with integer labels.

        // The integer value of each case corresponds to the index of each label
        // string in the sorted array of label strings.

        // The value of the integer condition is obtained by calling the druntime template
        // switch(object.__switch(cond, options...)) {0: {...}, 1: {...}, ...}

        // We sort a copy of the array of labels because we want to do a binary search in object.__switch,
        // without modifying the order of the case blocks here in the compiler.

        if (!verifyHookExist(ss.loc, *sc, Id.__switch, "switch cases on strings"))
            return setError();

        size_t numcases = 0;
        if (ss.cases)
            numcases = ss.cases.length;

        for (size_t i = 0; i < numcases; i++)
        {
            CaseStatement cs = (*ss.cases)[i];
            cs.index = cast(int)i;
        }

        // Make a copy of all the cases so that qsort doesn't scramble the actual
        // data we pass to codegen (the order of the cases in the switch).
        CaseStatements *csCopy = (*ss.cases).copy();

        if (numcases)
        {
            static int sort_compare(in CaseStatement* x, in CaseStatement* y) @trusted
            {
                auto se1 = x.exp.isStringExp();
                auto se2 = y.exp.isStringExp();
                return (se1 && se2) ? se1.compare(se2) : 0;
            }
            // Sort cases for efficient lookup
            csCopy.sort!sort_compare;
        }

        // The actual lowering
        auto arguments = new Expressions();
        arguments.push(ss.condition);

        auto compileTimeArgs = new Objects();

        // The type & label no.
        compileTimeArgs.push(new TypeExp(ss.loc, ss.condition.type.nextOf()));

        // The switch labels
        foreach (caseString; *csCopy)
        {
            compileTimeArgs.push(caseString.exp);
        }

        Expression sl = new IdentifierExp(ss.loc, Id.empty);
        sl = new DotIdExp(ss.loc, sl, Id.object);
        sl = new DotTemplateInstanceExp(ss.loc, sl, Id.__switch, compileTimeArgs);

        sl = new CallExp(ss.loc, sl, arguments);
        sl = sl.expressionSemantic(sc);
        ss.condition = sl;

        auto i = 0;
        foreach (c; *csCopy)
        {
            (*ss.cases)[c.index].exp = new IntegerExp(i++);
        }

        //printf("%s\n", ss._body.toChars());
        ss.statementSemantic(sc);

        sc.pop();
        result = ss;
    }

    void visitCase(CaseStatement cs)
    {
        SwitchStatement sw = sc.sw;
        bool errors = false;

        //printf("CaseStatement::semantic() %s\n", toChars());
        sc = sc.startCTFE();
        cs.exp = cs.exp.expressionSemantic(sc);
        cs.exp = resolveProperties(sc, cs.exp);
        sc = sc.endCTFE();

        if (sw)
        {
            Expression initialExp = cs.exp;

            // The switch'ed value has errors and doesn't provide the actual type
            // Omit the cast to enable further semantic (exluding the check for matching types)
            if (sw.condition.type && !sw.condition.type.isTypeError())
                cs.exp = cs.exp.implicitCastTo(sc, sw.condition.type);
            cs.exp = cs.exp.optimize(WANTvalue | WANTexpand);

            Expression e = cs.exp;
            // Remove all the casts the user and/or implicitCastTo may introduce
            // otherwise we'd sometimes fail the check below.
            while (e.isCastExp())
                e = e.isCastExp().e1;

            /* This is where variables are allowed as case expressions.
            */
            if (auto ve = e.isVarExp())
            {
                VarDeclaration v = ve.var.isVarDeclaration();
                Type t = cs.exp.type.toBasetype();
                if (v && (t.isIntegral() || t.ty == Tclass))
                {
                    /* Flag that we need to do special code generation
                    * for this, i.e. generate a sequence of if-then-else
                    */
                    sw.hasVars = true;

                    /* TODO check if v can be uninitialized at that point.
                    */
                    if (!v.isConst() && !v.isImmutable())
                    {
                        error(cs.loc, "`case` variables have to be `const` or `immutable`");
                    }

                    if (sw.isFinal)
                    {
                        error(cs.loc, "`case` variables not allowed in `final switch` statements");
                        errors = true;
                    }

                    /* Find the outermost scope `scx` that set `sw`.
                    * Then search scope `scx` for a declaration of `v`.
                    */
                    for (Scope* scx = sc; scx; scx = scx.enclosing)
                    {
                        if (scx.enclosing && scx.enclosing.sw == sw)
                            continue;
                        assert(scx.sw == sw);

                        Dsymbol pscopesym;
                        if (!scx.search(cs.exp.loc, v.ident, pscopesym))
                        {
                            error(cs.loc, "`case` variable `%s` declared at %s cannot be declared in `switch` body",
                                v.toChars(), v.loc.toChars());
                            errors = true;
                        }
                        break;
                    }
                    goto L1;
                }
            }
            else
                cs.exp = cs.exp.ctfeInterpret();

            if (StringExp se = cs.exp.toStringExp())
                cs.exp = se;
            else if (!cs.exp.isIntegerExp() && !cs.exp.isErrorExp())
            {
                error(cs.loc, "`case` expression must be a compile-time `string` or an integral constant, not `%s`", cs.exp.toChars());
                errors = true;
            }

        L1:
            // // Don't check other cases if this has errors
            if (!cs.exp.isErrorExp())
            foreach (cs2; *sw.cases)
            {
                //printf("comparing '%s' with '%s'\n", exp.toChars(), cs.exp.toChars());
                if (cs2.exp.equals(cs.exp))
                {
                    // https://issues.dlang.org/show_bug.cgi?id=15909
                    error(cs.loc, "duplicate `case %s` in `switch` statement", initialExp.toChars());
                    errors = true;
                    break;
                }
            }

            sw.cases.push(cs);

            // Resolve any goto case's with no exp to this case statement
            for (size_t i = 0; i < sw.gotoCases.length;)
            {
                GotoCaseStatement gcs = sw.gotoCases[i];
                if (!gcs.exp)
                {
                    gcs.cs = cs;
                    sw.gotoCases.remove(i); // remove from array
                    continue;
                }
                i++;
            }

            if (sc.sw.tf != sc.tf)
            {
                error(cs.loc, "`switch` and `case` are in different `finally` blocks");
                errors = true;
            }
            if (sc.sw.tryBody != sc.tryBody)
            {
                error(cs.loc, "case cannot be in different `try` block level from `switch`");
                errors = true;
            }
        }
        else
        {
            error(cs.loc, "`case` not in `switch` statement");
            errors = true;
        }

        sc.ctorflow.orCSX(CSX.label);
        cs.statement = cs.statement.statementSemantic(sc);
        if (cs.statement.isErrorStatement())
        {
            result = cs.statement;
            return;
        }
        if (errors || cs.exp.op == EXP.error)
            return setError();

        cs.lastVar = sc.lastVar;
        result = cs;
    }

    void visitCaseRange(CaseRangeStatement crs)
    {
        SwitchStatement sw = sc.sw;
        if (sw is null)
        {
            error(crs.loc, "case range not in `switch` statement");
            return setError();
        }

        //printf("CaseRangeStatement::semantic() %s\n", toChars());
        bool errors = false;
        if (sw.isFinal)
        {
            error(crs.loc, "case ranges not allowed in `final switch`");
            errors = true;
        }

        sc = sc.startCTFE();
        crs.first = crs.first.expressionSemantic(sc);
        crs.first = resolveProperties(sc, crs.first);
        sc = sc.endCTFE();
        crs.first = crs.first.implicitCastTo(sc, sw.condition.type);
        crs.first = crs.first.ctfeInterpret();

        sc = sc.startCTFE();
        crs.last = crs.last.expressionSemantic(sc);
        crs.last = resolveProperties(sc, crs.last);
        sc = sc.endCTFE();
        crs.last = crs.last.implicitCastTo(sc, sw.condition.type);
        crs.last = crs.last.ctfeInterpret();

        if (crs.first.op == EXP.error || crs.last.op == EXP.error || errors)
        {
            if (crs.statement)
                crs.statement.statementSemantic(sc);
            return setError();
        }

        uinteger_t fval = crs.first.toInteger();
        uinteger_t lval = crs.last.toInteger();
        if ((crs.first.type.isUnsigned() && fval > lval) || (!crs.first.type.isUnsigned() && cast(sinteger_t)fval > cast(sinteger_t)lval))
        {
            error(crs.loc, "first `case %s` is greater than last `case %s`", crs.first.toChars(), crs.last.toChars());
            errors = true;
            lval = fval;
        }

        if (lval - fval > 256)
        {
            error(crs.loc, "had %llu cases which is more than 257 cases in case range", 1 + lval - fval);
            errors = true;
            lval = fval + 256;
        }

        if (errors)
            return setError();

        /* This works by replacing the CaseRange with an array of Case's.
         *
         * case a: .. case b: s;
         *    =>
         * case a:
         *   [...]
         * case b:
         *   s;
         */

        auto statements = new Statements();
        for (uinteger_t i = fval; i != lval + 1; i++)
        {
            Statement s = crs.statement;
            if (i != lval) // if not last case
                s = new ExpStatement(crs.loc, cast(Expression)null);
            Expression e = new IntegerExp(crs.loc, i, crs.first.type);
            Statement cs = new CaseStatement(crs.loc, e, s);
            statements.push(cs);
        }
        Statement s = new CompoundStatement(crs.loc, statements);
        sc.ctorflow.orCSX(CSX.label);
        s = s.statementSemantic(sc);
        result = s;
    }

    void visitDefault(DefaultStatement ds)
    {
        //printf("DefaultStatement::semantic()\n");
        bool errors = false;
        if (sc.sw)
        {
            if (sc.sw.sdefault)
            {
                error(ds.loc, "`switch` statement already has a default");
                errors = true;
            }
            sc.sw.sdefault = ds;

            if (sc.sw.tf != sc.tf)
            {
                error(ds.loc, "`switch` and `default` are in different `finally` blocks");
                errors = true;
            }
            if (sc.sw.tryBody != sc.tryBody)
            {
                error(ds.loc, "default cannot be in different `try` block level from `switch`");
                errors = true;
            }
            if (sc.sw.isFinal)
            {
                error(ds.loc, "`default` statement not allowed in `final switch` statement");
                errors = true;
            }
        }
        else
        {
            error(ds.loc, "`default` not in `switch` statement");
            errors = true;
        }

        sc.ctorflow.orCSX(CSX.label);
        ds.statement = ds.statement.statementSemantic(sc);
        if (errors || ds.statement.isErrorStatement())
            return setError();

        ds.lastVar = sc.lastVar;
        result = ds;
    }

    void visitGotoDefault(GotoDefaultStatement gds)
    {
        /* https://dlang.org/spec/statement.html#goto-statement
         */

        gds.sw = sc.sw;
        if (!gds.sw)
        {
            error(gds.loc, "`goto default` not in `switch` statement");
            return setError();
        }
        if (gds.sw.isFinal)
        {
            error(gds.loc, "`goto default` not allowed in `final switch` statement");
            return setError();
        }
        result = gds;
    }

    void visitGotoCase(GotoCaseStatement gcs)
    {
        /* https://dlang.org/spec/statement.html#goto-statement
         */

        if (!sc.sw)
        {
            error(gcs.loc, "`goto case` not in `switch` statement");
            return setError();
        }

        if (gcs.exp)
        {
            gcs.exp = gcs.exp.expressionSemantic(sc);
            gcs.exp = gcs.exp.implicitCastTo(sc, sc.sw.condition.type);
            gcs.exp = gcs.exp.optimize(WANTvalue);
            if (gcs.exp.op == EXP.error)
                return setError();
        }

        sc.sw.gotoCases.push(gcs);
        result = gcs;
    }

    void visitReturn(ReturnStatement rs)
    {
        /* https://dlang.org/spec/statement.html#return-statement
         */

        //printf("ReturnStatement.dsymbolSemantic() %s\n", toChars(rs));

        FuncDeclaration fd = sc.parent.isFuncDeclaration();
        if (fd.fes)
            fd = fd.fes.func; // fd is now function enclosing foreach

        auto tf = fd.type.isTypeFunction();

        if (rs.exp && rs.exp.isVarExp() && rs.exp.isVarExp().var == fd.vresult)
        {
            // return vresult;
            if (sc.fes)
            {
                assert(rs.caseDim == 0);
                sc.fes.cases.push(rs);
                result = new ReturnStatement(Loc.initial, new IntegerExp(sc.fes.cases.length + 1));
                return;
            }
            if (fd.returnLabel)
            {
                auto gs = new GotoStatement(rs.loc, Id.returnLabel);
                gs.label = fd.returnLabel;
                result = gs;
                return;
            }

            if (!fd.returns)
                fd.returns = new ReturnStatements();
            fd.returns.push(rs);
            result = rs;
            return;
        }

        Type tret = tf.next;
        Type tbret = tret ? tret.toBasetype() : null;

        bool inferRef = (tf.isRef && (fd.storage_class & STC.auto_));
        Expression e0 = null;

        bool errors = false;
        if (sc.contract)
        {
            error(rs.loc, "`return` statements cannot be in contracts");
            errors = true;
        }
        if (sc.os)
        {
            // @@@DEPRECATED_2.112@@@
            // Deprecated in 2.100, transform into an error in 2.112
            if (sc.os.tok == TOK.onScopeFailure)
            {
                deprecation(rs.loc, "`return` statements cannot be in `scope(failure)` bodies.");
                deprecationSupplemental(rs.loc, "Use try-catch blocks for this purpose");
            }
            else
            {
                error(rs.loc, "`return` statements cannot be in `%s` bodies", Token.toChars(sc.os.tok));
                errors = true;
            }
        }
        if (sc.tf)
        {
            error(rs.loc, "`return` statements cannot be in `finally` bodies");
            errors = true;
        }

        if (fd.isCtorDeclaration())
        {
            if (rs.exp)
            {
                error(rs.loc, "cannot return expression from constructor");
                errors = true;
            }

            // Constructors implicitly do:
            //      return this;
            rs.exp = new ThisExp(Loc.initial);
            rs.exp.type = tret;
        }
        else if (rs.exp)
        {
            fd.hasMultipleReturnExp = fd.hasReturnExp;
            fd.hasReturnExp = true;

            FuncLiteralDeclaration fld = fd.isFuncLiteralDeclaration();
            if (tret)
                rs.exp = inferType(rs.exp, tret);
            else if (fld && fld.treq)
                rs.exp = inferType(rs.exp, fld.treq.nextOf().nextOf());

            rs.exp = rs.exp.expressionSemantic(sc);
            rs.exp = rs.exp.arrayFuncConv(sc);
            // If we're returning by ref, allow the expression to be `shared`
            const returnSharedRef = (tf.isRef && (fd.inferRetType || tret.isShared()));
            rs.exp.checkSharedAccess(sc, returnSharedRef);

            // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
            if (rs.exp.isTypeExp())
                rs.exp = resolveAliasThis(sc, rs.exp);

            rs.exp = resolveProperties(sc, rs.exp);
            if (rs.exp.checkType())
                rs.exp = ErrorExp.get();
            if (auto f = isFuncAddress(rs.exp))
            {
                if (fd.inferRetType && f.checkForwardRef(rs.exp.loc))
                    rs.exp = ErrorExp.get();
            }
            if (checkNonAssignmentArrayOp(rs.exp))
                rs.exp = ErrorExp.get();

            // Extract side-effect part
            rs.exp = Expression.extractLast(rs.exp, e0);
            if (rs.exp.isCallExp())
                rs.exp = valueNoDtor(rs.exp);

            /* Void-return function can have void / noreturn typed expression
             * on return statement.
             */
            auto texp = rs.exp.type;
            const convToVoid = texp.ty == Tvoid || texp.isTypeNoreturn();

            if (tbret && tbret.ty == Tvoid || convToVoid)
            {
                if (!convToVoid)
                {
                    error(rs.loc, "cannot return non-void from `void` function");
                    errors = true;
                    rs.exp = new CastExp(rs.loc, rs.exp, Type.tvoid);
                    rs.exp = rs.exp.expressionSemantic(sc);
                }

                // https://issues.dlang.org/show_bug.cgi?id=23063
                rs.exp = checkNoreturnVarAccess(rs.exp);

                // @@@DEPRECATED_2.111@@@
                const olderrors = global.startGagging();
                // uncomment to turn deprecation into an error when
                // deprecation cycle is over
                if (discardValue(rs.exp))
                {
                    //errors = true;
                }
                if (global.endGagging(olderrors))
                    deprecation(rs.exp.loc, "`%s` has no effect", rs.exp.toChars());

                /* Replace:
                 *      return exp;
                 * with:
                 *      exp; return;
                 */
                e0 = Expression.combine(e0, rs.exp);
                rs.exp = null;
            }
            if (e0)
            {
                e0 = e0.optimize(WANTvalue);
                e0 = checkGC(sc, e0);
            }
        }

        if (rs.exp)
        {
            if (fd.inferRetType) // infer return type
            {
                if (!tret)
                {
                    tf.next = rs.exp.type;
                }
                else if (!tret.isTypeError() && !rs.exp.type.equals(tret))
                {
                    int m1 = rs.exp.type.implicitConvTo(tret);
                    int m2 = tret.implicitConvTo(rs.exp.type);
                    //printf("exp.type = %s m2<-->m1 tret %s\n", exp.type.toChars(), tret.toChars());
                    //printf("m1 = %d, m2 = %d\n", m1, m2);

                    if (m1 && m2)
                    {
                    }
                    else if (!m1 && m2)
                        tf.next = rs.exp.type;
                    else if (m1 && !m2)
                    {
                    }
                    else if (!rs.exp.isErrorExp())
                    {
                        error(rs.loc, "expected return type of `%s`, not `%s`:",
                                 tret.toChars(),
                                 rs.exp.type.toChars());
                        errorSupplemental((fd.returns) ? (*fd.returns)[0].loc : fd.loc,
                                          "Return type of `%s` inferred here.",
                                          tret.toChars());

                        errors = true;
                        tf.next = Type.terror;
                    }
                }

                tret = tf.next;
                tbret = tret.toBasetype();
            }

            if (inferRef) // deduce 'auto ref'
            {
                /* Determine "refness" of function return:
                 * if it's an lvalue, return by ref, else return by value
                 * https://dlang.org/spec/function.html#auto-ref-functions
                 */

                void turnOffRef(scope void delegate() supplemental)
                {
                    tf.isRef = false;    // return by value
                    tf.isReturn = false; // ignore 'return' attribute, whether explicit or inferred
                    fd.storage_class &= ~STC.return_;

                    // If we previously assumed the function could be ref when
                    // checking for `shared`, make sure we were right
                    if (sc.previews.noSharedAccess && rs.exp.type.isShared())
                    {
                        .error(fd.loc, "%s `%s` function returns `shared` but cannot be inferred `ref`", fd.kind, fd.toPrettyChars);
                        supplemental();
                    }
                }

                if (rs.exp.isLvalue())
                {
                    /* May return by ref
                     */
                    Scope* sc2 = sc.push();
                    sc2.eSink = global.errorSinkNull;
                    bool err = checkReturnEscapeRef(*sc2, rs.exp, true);
                    sc2.pop();

                    if (err)
                        turnOffRef(() { checkReturnEscapeRef(*sc, rs.exp, false); });
                    else if (!rs.exp.type.constConv(tf.next))
                        turnOffRef(
                            () => rs.loc.errorSupplemental("cannot implicitly convert `%s` of type `%s` to `%s`",
                                      rs.exp.toChars(), rs.exp.type.toChars(), tf.next.toChars())
                        );
                }
                else
                    turnOffRef(
                        () => rs.loc.errorSupplemental("return value `%s` is not an lvalue", rs.exp.toChars())
                    );

                /* The "refness" is determined by all of return statements.
                 * This means:
                 *    return 3; return x;  // ok, x can be a value
                 *    return x; return 3;  // ok, x can be a value
                 */
            }
        }
        else
        {
            // Type of the returned expression (if any), might've been moved to e0
            auto resType = e0 ? e0.type : Type.tvoid;

            // infer return type
            if (fd.inferRetType)
            {
                // 1. First `return <noreturn exp>?`
                // 2. Potentially found a returning branch, update accordingly
                if (!tf.next || tf.next.toBasetype().isTypeNoreturn())
                {
                    tf.next = resType; // infer void or noreturn
                }
                // Found an actual return value before
                else if (tf.next.ty != Tvoid && !resType.toBasetype().isTypeNoreturn())
                {
                    if (!tf.next.isTypeError())
                    {
                        error(rs.loc, "mismatched function return type inference of `void` and `%s`", tf.next.toChars());
                    }
                    errors = true;
                    tf.next = Type.terror;
                }

                tret = tf.next;
                tbret = tret.toBasetype();
            }

            // https://issues.dlang.org/show_bug.cgi?id=23914
            if (inferRef && !resType.isTypeNoreturn()) // deduce 'auto ref'
                tf.isRef = false;

            if (tbret.ty != Tvoid && !resType.isTypeNoreturn()) // if non-void return
            {
                if (!tbret.isTypeError())
                {
                    if (e0)
                        error(rs.loc, "expected return type of `%s`, not `%s`", tret.toChars(), resType.toChars());
                    else if (tbret.isTypeNoreturn())
                    {
                        error(rs.loc, "cannot return from `noreturn` function");
                        .errorSupplemental(rs.loc,
                            "Consider adding an endless loop, `assert(0)`, or another `noreturn` expression");
                    }
                    else
                        error(rs.loc, "`return` expression expected");
                }
                errors = true;
            }
            else if (fd.isMain())
            {
                // main() returns 0, even if it returns void
                rs.exp = IntegerExp.literal!0;
            }
        }

        // If any branches have called a ctor, but this branch hasn't, it's an error
        if (sc.ctorflow.callSuper & CSX.any_ctor && !(sc.ctorflow.callSuper & (CSX.this_ctor | CSX.super_ctor)))
        {
            error(rs.loc, "`return` without calling constructor");
            errors = true;
        }

        if (sc.ctorflow.fieldinit.length)       // if aggregate fields are being constructed
        {
            auto ad = fd.isMemberLocal();
            assert(ad);
            foreach (i, v; ad.fields)
            {
                bool mustInit = (v.storage_class & STC.nodefaultctor || v.type.needsNested());
                if (mustInit && !(sc.ctorflow.fieldinit[i].csx & CSX.this_ctor))
                {
                    error(rs.loc, "an earlier `return` statement skips field `%s` initialization", v.toChars());
                    errors = true;
                }
            }
        }
        sc.ctorflow.orCSX(CSX.return_);

        if (errors)
            return setError();

        if (sc.fes)
        {
            if (!rs.exp)
            {
                // Send out "case receiver" statement to the foreach.
                //  return exp;
                Statement s = new ReturnStatement(Loc.initial, rs.exp);
                sc.fes.cases.push(s);

                // Immediately rewrite "this" return statement as:
                //  return cases.length+1;
                rs.exp = new IntegerExp(sc.fes.cases.length + 1);
                if (e0)
                {
                    result = new CompoundStatement(rs.loc, new ExpStatement(rs.loc, e0), rs);
                    return;
                }
                result = rs;
                return;
            }
            else
            {
                fd.buildResultVar(null, rs.exp.type);
                bool r = fd.vresult.checkNestedReference(sc, Loc.initial);
                assert(!r); // vresult should be always accessible

                // Send out "case receiver" statement to the foreach.
                //  return vresult;
                Statement s = new ReturnStatement(Loc.initial, new VarExp(Loc.initial, fd.vresult));
                sc.fes.cases.push(s);

                // Save receiver index for the later rewriting from:
                //  return exp;
                // to:
                //  vresult = exp; retrun caseDim;
                rs.caseDim = sc.fes.cases.length + 1;
            }
        }
        if (rs.exp)
        {
            if (!fd.returns)
                fd.returns = new ReturnStatements();
            fd.returns.push(rs);
        }
        if (e0)
        {
            if (e0.isDeclarationExp() || e0.isCommaExp())
            {
                rs.exp = Expression.combine(e0, rs.exp);
            }
            else
            {
                auto es = new ExpStatement(rs.loc, e0);
                if (e0.type.isTypeNoreturn())
                    result = es; // Omit unreachable return;
                else
                    result = new CompoundStatement(rs.loc, es, rs);

                return;
            }
        }
        result = rs;
    }

    void visitBreak(BreakStatement bs)
    {
        /* https://dlang.org/spec/statement.html#break-statement
         */

        //printf("BreakStatement::semantic()\n");

        // If:
        //  break Identifier;
        if (bs.ident)
        {
            bs.ident = fixupLabelName(sc, bs.ident);

            FuncDeclaration thisfunc = sc.func;

            for (Scope* scx = sc; scx; scx = scx.enclosing)
            {
                if (scx.func != thisfunc) // if in enclosing function
                {
                    if (sc.fes) // if this is the body of a foreach
                    {
                        /* Post this statement to the fes, and replace
                         * it with a return value that caller will put into
                         * a switch. Caller will figure out where the break
                         * label actually is.
                         * Case numbers start with 2, not 0, as 0 is continue
                         * and 1 is break.
                         */
                        sc.fes.cases.push(bs);
                        result = new ReturnStatement(Loc.initial, new IntegerExp(sc.fes.cases.length + 1));
                        return;
                    }
                    break; // can't break to it
                }

                LabelStatement ls = scx.slabel;
                if (ls && ls.ident == bs.ident)
                {
                    Statement s = ls.statement;
                    if (!s || !s.hasBreak())
                        error(bs.loc, "label `%s` has no `break`", bs.ident.toChars());
                    else if (ls.tf != sc.tf)
                        error(bs.loc, "cannot break out of `finally` block");
                    else
                    {
                        ls.breaks = true;
                        result = bs;
                        return;
                    }
                    return setError();
                }
            }
            error(bs.loc, "enclosing label `%s` for `break` not found", bs.ident.toChars());
            return setError();
        }
        else if (!sc.sbreak)
        {
            if (sc.os && sc.os.tok != TOK.onScopeFailure)
            {
                error(bs.loc, "`break` is not allowed inside `%s` bodies", Token.toChars(sc.os.tok));
            }
            else if (sc.fes)
            {
                // Replace break; with return 1;
                result = new ReturnStatement(Loc.initial, IntegerExp.literal!1);
                return;
            }
            else
                error(bs.loc, "`break` is not inside a loop or `switch`");
            return setError();
        }
        else if (sc.sbreak.isForwardingStatement())
        {
            error(bs.loc, "must use labeled `break` within `static foreach`");
        }
        result = bs;
    }

    void visitContinue(ContinueStatement cs)
    {
        /* https://dlang.org/spec/statement.html#continue-statement
         */

        //printf("ContinueStatement::semantic() %p\n", cs);
        if (cs.ident)
        {
            cs.ident = fixupLabelName(sc, cs.ident);

            Scope* scx;
            FuncDeclaration thisfunc = sc.func;

            for (scx = sc; scx; scx = scx.enclosing)
            {
                LabelStatement ls;
                if (scx.func != thisfunc) // if in enclosing function
                {
                    if (sc.fes) // if this is the body of a foreach
                    {
                        for (; scx; scx = scx.enclosing)
                        {
                            ls = scx.slabel;
                            if (ls && ls.ident == cs.ident && ls.statement == sc.fes)
                            {
                                // Replace continue ident; with return 0;
                                result = new ReturnStatement(Loc.initial, IntegerExp.literal!0);
                                return;
                            }
                        }

                        /* Post this statement to the fes, and replace
                         * it with a return value that caller will put into
                         * a switch. Caller will figure out where the break
                         * label actually is.
                         * Case numbers start with 2, not 0, as 0 is continue
                         * and 1 is break.
                         */
                        sc.fes.cases.push(cs);
                        result = new ReturnStatement(Loc.initial, new IntegerExp(sc.fes.cases.length + 1));
                        return;
                    }
                    break; // can't continue to it
                }

                ls = scx.slabel;
                if (ls && ls.ident == cs.ident)
                {
                    Statement s = ls.statement;
                    if (!s || !s.hasContinue())
                        error(cs.loc, "label `%s` has no `continue`", cs.ident.toChars());
                    else if (ls.tf != sc.tf)
                        error(cs.loc, "cannot continue out of `finally` block");
                    else
                    {
                        result = cs;
                        return;
                    }
                    return setError();
                }
            }
            error(cs.loc, "enclosing label `%s` for `continue` not found", cs.ident.toChars());
            return setError();
        }
        else if (!sc.scontinue)
        {
            if (sc.os && sc.os.tok != TOK.onScopeFailure)
            {
                error(cs.loc, "`continue` is not allowed inside `%s` bodies", Token.toChars(sc.os.tok));
            }
            else if (sc.fes)
            {
                // Replace continue; with return 0;
                result = new ReturnStatement(Loc.initial, IntegerExp.literal!0);
                return;
            }
            else
                error(cs.loc, "`continue` is not inside a loop");
            return setError();
        }
        else if (sc.scontinue.isForwardingStatement())
        {
            error(cs.loc, "must use labeled `continue` within `static foreach`");
        }
        result = cs;
    }

    void visitSynchronized(SynchronizedStatement ss)
    {
        /* https://dlang.org/spec/statement.html#synchronized-statement
         */

        if (ss.exp)
        {
            ss.exp = ss.exp.expressionSemantic(sc);
            ss.exp = resolveProperties(sc, ss.exp);
            ss.exp = ss.exp.optimize(WANTvalue);
            ss.exp = checkGC(sc, ss.exp);
            if (ss.exp.op == EXP.error)
            {
                if (ss._body)
                    ss._body = ss._body.statementSemantic(sc);
                return setError();
            }

            ClassDeclaration cd = ss.exp.type.isClassHandle();
            if (!cd)
            {
                error(ss.loc, "can only `synchronize` on class objects, not `%s`", ss.exp.type.toChars());
                return setError();
            }
            else if (cd.isInterfaceDeclaration())
            {
                /* Cast the interface to an object, as the object has the monitor,
                 * not the interface.
                 */
                if (!ClassDeclaration.object)
                {
                    ObjectNotFound(ss.loc, Id.Object);
                    return setError();
                }

                Type t = ClassDeclaration.object.type;
                t = t.typeSemantic(Loc.initial, sc).toBasetype();
                assert(t.ty == Tclass);

                ss.exp = new CastExp(ss.loc, ss.exp, t);
                ss.exp = ss.exp.expressionSemantic(sc);
            }
            version (all)
            {
                /* Rewrite as:
                 *  auto tmp = exp;
                 *  _d_monitorenter(tmp);
                 *  try { body } finally { _d_monitorexit(tmp); }
                 */
                auto tmp = copyToTemp(0, "__sync", ss.exp);
                tmp.dsymbolSemantic(sc);

                auto cs = new Statements();
                cs.push(new ExpStatement(ss.loc, tmp));

                auto args = new Parameters();
                args.push(new Parameter(Loc.initial, 0, ClassDeclaration.object.type, null, null, null));

                FuncDeclaration fdenter = FuncDeclaration.genCfunc(args, Type.tvoid, Id.monitorenter);
                Expression e = new CallExp(ss.loc, fdenter, new VarExp(ss.loc, tmp));
                e.type = Type.tvoid; // do not run semantic on e

                cs.push(new ExpStatement(ss.loc, e));
                FuncDeclaration fdexit = FuncDeclaration.genCfunc(args, Type.tvoid, Id.monitorexit);
                e = new CallExp(ss.loc, fdexit, new VarExp(ss.loc, tmp));
                e.type = Type.tvoid; // do not run semantic on e
                Statement s = new ExpStatement(ss.loc, e);
                s = new TryFinallyStatement(ss.loc, ss._body, s);
                cs.push(s);

                s = new CompoundStatement(ss.loc, cs);
                result = s.statementSemantic(sc);
            }
        }
        else
        {
            /* Generate our own critical section, then rewrite as:
             *  static shared void* __critsec;
             *  _d_criticalenter2(&__critsec);
             *  try { body } finally { _d_criticalexit(__critsec); }
             */
            auto id = Identifier.generateId("__critsec");
            auto t = Type.tvoidptr;
            auto tmp = new VarDeclaration(ss.loc, t, id, null);
            tmp.storage_class |= STC.temp | STC.shared_ | STC.static_;
            Expression tmpExp = new VarExp(ss.loc, tmp);

            auto cs = new Statements();
            cs.push(new ExpStatement(ss.loc, tmp));

            /* This is just a dummy variable for "goto skips declaration" error.
             * Backend optimizer could remove this unused variable.
             */
            auto v = new VarDeclaration(ss.loc, Type.tvoidptr, Identifier.generateId("__sync"), null);
            v.dsymbolSemantic(sc);
            cs.push(new ExpStatement(ss.loc, v));

            auto enterArgs = new Parameters();
            enterArgs.push(new Parameter(Loc.initial, 0, t.pointerTo(), null, null, null));

            FuncDeclaration fdenter = FuncDeclaration.genCfunc(enterArgs, Type.tvoid, Id.criticalenter, STC.nothrow_);
            Expression e = new AddrExp(ss.loc, tmpExp);
            e = e.expressionSemantic(sc);
            e = new CallExp(ss.loc, fdenter, e);
            e.type = Type.tvoid; // do not run semantic on e
            cs.push(new ExpStatement(ss.loc, e));

            auto exitArgs = new Parameters();
            exitArgs.push(new Parameter(Loc.initial, 0, t, null, null, null));

            FuncDeclaration fdexit = FuncDeclaration.genCfunc(exitArgs, Type.tvoid, Id.criticalexit, STC.nothrow_);
            e = new CallExp(ss.loc, fdexit, tmpExp);
            e.type = Type.tvoid; // do not run semantic on e
            Statement s = new ExpStatement(ss.loc, e);
            s = new TryFinallyStatement(ss.loc, ss._body, s);
            cs.push(s);

            s = new CompoundStatement(ss.loc, cs);
            result = s.statementSemantic(sc);
        }
    }

    void visitWith(WithStatement ws)
    {
        /* https://dlang.org/spec/statement.html#with-statement
         */

        ScopeDsymbol sym;
        Initializer _init;

        //printf("WithStatement::semantic()\n");
        ws.exp = ws.exp.expressionSemantic(sc);
        ws.exp = resolveProperties(sc, ws.exp);
        ws.exp = ws.exp.optimize(WANTvalue);
        ws.exp = checkGC(sc, ws.exp);
        if (ws.exp.op == EXP.error)
            return setError();
        if (ws.exp.op == EXP.scope_)
        {
            sym = new WithScopeSymbol(ws);
            sym.parent = sc.scopesym;
            sym.endlinnum = ws.endloc.linnum;
        }
        else if (auto et = ws.exp.isTypeExp())
        {
            Dsymbol s = et.type.toDsymbol(sc);
            if (!s || !s.isScopeDsymbol())
            {
                error(ws.loc, "`with` type `%s` has no members", ws.exp.toChars());
                return setError();
            }
            sym = new WithScopeSymbol(ws);
            sym.parent = sc.scopesym;
            sym.endlinnum = ws.endloc.linnum;
        }
        else
        {
            Type texp = ws.exp.type;
            Type t = texp.toBasetype();

            Expression olde = ws.exp;
            if (t.ty == Tpointer)
            {
                ws.exp = new PtrExp(ws.loc, ws.exp);
                ws.exp = ws.exp.expressionSemantic(sc);
                texp = ws.exp.type;
                t = texp.toBasetype();
            }

            assert(t);
            t = t.toBasetype();
            if (t.isClassHandle())
            {
                _init = new ExpInitializer(ws.loc, ws.exp);
                ws.wthis = new VarDeclaration(ws.loc, ws.exp.type, Id.withSym, _init);
                ws.wthis.storage_class |= STC.temp;
                ws.wthis.dsymbolSemantic(sc);

                sym = new WithScopeSymbol(ws);
                sym.parent = sc.scopesym;
                sym.endlinnum = ws.endloc.linnum;
            }
            else if (t.ty == Tstruct)
            {
                if (!ws.exp.isLvalue())
                {
                    /* Re-write to
                     * {
                     *   auto __withtmp = exp
                     *   with(__withtmp)
                     *   {
                     *     ...
                     *   }
                     * }
                     */
                    auto tmp = copyToTemp(0, "__withtmp", ws.exp);
                    tmp.dsymbolSemantic(sc);
                    auto es = new ExpStatement(ws.loc, tmp);
                    ws.exp = new VarExp(ws.loc, tmp);
                    Statement ss = new ScopeStatement(ws.loc, new CompoundStatement(ws.loc, es, ws), ws.endloc);
                    result = ss.statementSemantic(sc);
                    return;
                }
                Expression e = ws.exp.addressOf();
                _init = new ExpInitializer(ws.loc, e);
                ws.wthis = new VarDeclaration(ws.loc, e.type, Id.withSym, _init);
                ws.wthis.storage_class |= STC.temp;
                ws.wthis.dsymbolSemantic(sc);
                sym = new WithScopeSymbol(ws);
                // Need to set the scope to make use of resolveAliasThis
                sym.setScope(sc);
                sym.parent = sc.scopesym;
                sym.endlinnum = ws.endloc.linnum;
            }
            else if (auto tenum = texp.isTypeEnum())
            {
                ws.exp = new TypeExp(ws.exp.loc, tenum);
                sym = new WithScopeSymbol(ws);
                sym.parent = sc.scopesym;
                sym.endlinnum = ws.endloc.linnum;
            }
            else
            {
                error(ws.loc, "`with` expression types must be enums or aggregates or pointers to them, not `%s`", olde.type.toChars());
                return setError();
            }
        }

        if (ws._body)
        {
            sym._scope = sc;
            sc = sc.push(sym);
            sc.insert(sym);
            ws._body = ws._body.statementSemantic(sc);
            sc.pop();
            if (ws._body && ws._body.isErrorStatement())
            {
                result = ws._body;
                return;
            }
        }

        result = ws;
    }

    // https://dlang.org/spec/statement.html#TryStatement
    void visitTryCatch(TryCatchStatement tcs)
    {
        //printf("TryCatchStatement.semantic()\n");

        if (!global.params.useExceptions)
        {
            error(tcs.loc, "cannot use try-catch statements with %s", global.params.betterC ? "-betterC".ptr : "-nothrow".ptr);
            return setError();
        }

        if (!ClassDeclaration.throwable)
        {
            error(tcs.loc, "cannot use try-catch statements because `object.Throwable` was not declared");
            return setError();
        }

        uint flags;
        enum FLAGcpp = 1;
        enum FLAGd = 2;

        tcs.tryBody = sc.tryBody;   // chain on the in-flight tryBody
        tcs._body = tcs._body.semanticScope(sc, null, null, tcs);

        /* Even if body is empty, still do semantic analysis on catches
         */
        bool catchErrors = false;
        foreach (i, c; *tcs.catches)
        {
            c.catchSemantic(sc);
            if (c.errors)
            {
                catchErrors = true;
                continue;
            }
            auto cd = c.type.toBasetype().isClassHandle();
            flags |= cd.isCPPclass() ? FLAGcpp : FLAGd;

            // Determine if current catch 'hides' any previous catches
            foreach (j; 0 .. i)
            {
                Catch cj = (*tcs.catches)[j];
                const si = c.loc.toChars();
                const sj = cj.loc.toChars();
                if (c.type.toBasetype().implicitConvTo(cj.type.toBasetype()))
                {
                    error(tcs.loc, "`catch` at %s hides `catch` at %s", sj, si);
                    catchErrors = true;
                }
            }
        }

        if (sc.func)
        {
            sc.func.hasCatches = true;
            if (flags == (FLAGcpp | FLAGd))
            {
                error(tcs.loc, "cannot mix catching D and C++ exceptions in the same try-catch");
                catchErrors = true;
            }
        }

        if (catchErrors)
            return setError();

        // No actual code in the try (i.e. omitted any conditionally compiled code)
        // Could also be extended to check for hasCode
        if (!tcs._body)
            return;

        if (tcs._body.isErrorStatement())
        {
            result = tcs._body;
            return;
        }

        /* If the try body never throws, we can eliminate any catches
         * of recoverable exceptions.
         */
        if (!(tcs._body.blockExit(sc.func, null) & BE.throw_) && ClassDeclaration.exception)
        {
            foreach_reverse (i; 0 .. tcs.catches.length)
            {
                Catch c = (*tcs.catches)[i];

                /* If catch exception type is derived from Exception
                 */
                if (c.type.toBasetype().implicitConvTo(ClassDeclaration.exception.type) &&
                    (!c.handler || !c.handler.comeFrom()) && !sc.debug_)
                {
                    // Remove c from the array of catches
                    tcs.catches.remove(i);
                }
            }
        }

        if (tcs.catches.length == 0)
        {
            result = tcs._body.hasCode() ? tcs._body : null;
            return;
        }

        result = tcs;
    }

    void visitTryFinally(TryFinallyStatement tfs)
    {
        //printf("TryFinallyStatement::semantic()\n");
        tfs.tryBody = sc.tryBody;   // chain on in-flight tryBody
        tfs._body = tfs._body.semanticScope(sc, null, null, tfs);

        sc = sc.push();
        sc.tf = tfs;
        sc.sbreak = null;
        sc.scontinue = null; // no break or continue out of finally block
        tfs.finalbody = tfs.finalbody.semanticNoScope(sc);
        sc.pop();

        if (!tfs._body)
        {
            result = tfs.finalbody;
            return;
        }
        if (!tfs.finalbody)
        {
            result = tfs._body;
            return;
        }

        auto blockexit = tfs._body.blockExit(sc.func, null);

        // if not worrying about exceptions
        if (!(global.params.useExceptions && ClassDeclaration.throwable))
            blockexit &= ~BE.throw_;            // don't worry about paths that otherwise may throw

        // Don't care about paths that halt, either
        if ((blockexit & ~BE.halt) == BE.fallthru)
        {
            result = new CompoundStatement(tfs.loc, tfs._body, tfs.finalbody);
            return;
        }
        tfs.bodyFallsThru = (blockexit & BE.fallthru) != 0;
        result = tfs;
    }

    void visitScopeGuard(ScopeGuardStatement oss)
    {
        /* https://dlang.org/spec/statement.html#scope-guard-statement
         */

        if (oss.tok != TOK.onScopeExit)
        {
            // https://issues.dlang.org/show_bug.cgi?id=23159
            if (!global.params.useExceptions)
            {
                version (IN_GCC)
                    error(oss.loc, "`%s` cannot be used with `-fno-exceptions`", Token.toChars(oss.tok));
                else
                    error(oss.loc, "`%s` cannot be used with -betterC", Token.toChars(oss.tok));
                return setError();
            }

            // scope(success) and scope(failure) are rewritten to try-catch(-finally) statement,
            // so the generated catch block cannot be placed in finally block.
            // See also Catch::semantic.
            if (sc.os && sc.os.tok != TOK.onScopeFailure)
            {
                // If enclosing is scope(success) or scope(exit), this will be placed in finally block.
                error(oss.loc, "cannot put `%s` statement inside `%s`", Token.toChars(oss.tok), Token.toChars(sc.os.tok));
                return setError();
            }
            if (sc.tf)
            {
                error(oss.loc, "cannot put `%s` statement inside `finally` block", Token.toChars(oss.tok));
                return setError();
            }
        }

        sc = sc.push();
        sc.tf = null;
        sc.os = oss;
        if (oss.tok != TOK.onScopeFailure)
        {
            // Jump out from scope(failure) block is allowed.
            sc.sbreak = null;
            sc.scontinue = null;
        }
        oss.statement = oss.statement.semanticNoScope(sc);
        sc.pop();

        if (!oss.statement || oss.statement.isErrorStatement())
        {
            result = oss.statement;
            return;
        }
        result = oss;
    }

    void visitThrow(ThrowStatement ts)
    {
        /* https://dlang.org/spec/statement.html#throw-statement
         */

        //printf("ThrowStatement::semantic()\n");
        if (throwSemantic(ts.loc, ts.exp, sc))
            result = ts;
        else
            setError();

    }

    void visitDebug(DebugStatement ds)
    {
        if (ds.statement)
        {
            sc = sc.push();
            sc.debug_ = true;
            ds.statement = ds.statement.statementSemantic(sc);
            sc.pop();
        }
        result = ds.statement;
    }

    void visitGoto(GotoStatement gs)
    {
        /* https://dlang.org/spec/statement.html#goto-statement
         */

        //printf("GotoStatement::semantic()\n");
        FuncDeclaration fd = sc.func;

        gs.ident = fixupLabelName(sc, gs.ident);
        gs.label = fd.searchLabel(gs.ident, gs.loc);
        gs.tryBody = sc.tryBody;
        gs.tf = sc.tf;
        gs.os = sc.os;
        gs.lastVar = sc.lastVar;
        gs.inCtfeBlock = sc.ctfeBlock;

        if (!gs.label.statement && sc.fes)
        {
            /* Either the goto label is forward referenced or it
             * is in the function that the enclosing foreach is in.
             * Can't know yet, so wrap the goto in a scope statement
             * so we can patch it later, and add it to a 'look at this later'
             * list.
             */
            gs.label.deleted = true;
            auto ss = new ScopeStatement(gs.loc, gs, gs.loc);
            sc.fes.gotos.push(ss); // 'look at this later' list
            result = ss;
            return;
        }

        // Add to fwdref list to check later
        if (!gs.label.statement)
        {
            if (!fd.gotos)
                fd.gotos = new GotoStatements();
            fd.gotos.push(gs);
        }
        else if (!sc.inCfile && gs.checkLabel())
            return setError();

        result = gs;
    }

    void visitLabel(LabelStatement ls)
    {
        //printf("LabelStatement::semantic()\n");
        FuncDeclaration fd = sc.parent.isFuncDeclaration();

        ls.ident = fixupLabelName(sc, ls.ident);
        ls.tryBody = sc.tryBody;
        ls.tf = sc.tf;
        ls.os = sc.os;
        ls.lastVar = sc.lastVar;
        ls.inCtfeBlock = sc.ctfeBlock;

        LabelDsymbol ls2 = fd.searchLabel(ls.ident, ls.loc);
        if (ls2.statement && !ls2.duplicated)
        {
            if (ls.loc == ls2.loc)
            {
                ls2.duplicated = true;
                error(ls.loc, "label `%s` is duplicated", ls2.toChars());
                .errorSupplemental(ls2.loc, "labels cannot be used in a static foreach with more than 1 iteration");
            }
            else
            {
                error(ls.loc, "label `%s` is already defined", ls2.toChars());
                .errorSupplemental(ls2.loc, "first definition is here");
            }
            return setError();
        }
        else
            ls2.statement = ls;

        sc = sc.push();
        sc.lastVar = sc.enclosing.lastVar;
        sc.scopesym = sc.enclosing.scopesym;

        sc.ctorflow.orCSX(CSX.label);

        sc.slabel = ls;
        if (ls.statement)
            ls.statement = ls.statement.statementSemantic(sc);

        //issue 24534: lastVar may have been updated in the nested scope
        sc.enclosing.lastVar = sc.lastVar;

        sc.pop();

        result = ls;
    }

    void visitAsm(AsmStatement s)
    {
        /* https://dlang.org/spec/statement.html#asm
         */

        //printf("AsmStatement()::semantic()\n");
        result = asmSemantic(s, sc);
    }

    void visitCompoundAsm(CompoundAsmStatement cas)
    {
        //printf("CompoundAsmStatement()::semantic()\n");
        // Apply postfix attributes of the asm block to each statement.
        sc = sc.push();
        sc.stc |= cas.stc;

        /* Go through the statements twice, first to declare any labels,
         * second for anything else.
         */

        foreach (ref s; *cas.statements)
        {
            if (s)
            {
                if (auto ls = s.isLabelStatement())
                {
                    sc.func.searchLabel(ls.ident, ls.loc);
                }
            }
        }

        foreach (ref s; *cas.statements)
        {
            s = s ? s.statementSemantic(sc) : null;
        }

        assert(sc.func);
        if (!(cas.stc & STC.pure_) && sc.func.setImpure(cas.loc, "`asm` statement is assumed to be impure - mark it with `pure` if it is not"))
            error(cas.loc, "`asm` statement is assumed to be impure - mark it with `pure` if it is not");
        if (!(cas.stc & STC.nogc) && sc.func.setGC(cas.loc, "`asm` statement in %s `%s` is assumed to use the GC - mark it with `@nogc` if it does not"))
            error(cas.loc, "`asm` statement is assumed to use the GC - mark it with `@nogc` if it does not");
        // @@@DEPRECATED_2.114@@@
        // change deprecation() to error(), add `else` and remove `| STC.safe`
        // to turn deprecation into an error when deprecation cycle is over
        if (cas.stc & STC.safe)
            deprecation(cas.loc, "`asm` statement cannot be marked `@safe`, use `@system` or `@trusted` instead");
        if (!(cas.stc & (STC.trusted | STC.safe)))
        {
            sc.setUnsafe(false, cas.loc, "`asm` statement without `@trusted` annotation");
        }

        sc.pop();
        result = cas;
    }

    void visitImport(ImportStatement imps)
    {
        /* https://dlang.org/spec/module.html#ImportDeclaration
         */

        foreach (i; 0 .. imps.imports.length)
        {
            Import s = (*imps.imports)[i].isImport();
            assert(!s.aliasdecls.length);
            foreach (j, name; s.names)
            {
                Identifier _alias = s.aliases[j];
                if (!_alias)
                    _alias = name;

                auto tname = new TypeIdentifier(s.loc, name);
                auto ad = new AliasDeclaration(s.loc, _alias, tname);
                ad._import = s;
                s.aliasdecls.push(ad);
            }

            s.dsymbolSemantic(sc);

            // https://issues.dlang.org/show_bug.cgi?id=19942
            // If the module that's being imported doesn't exist, don't add it to the symbol table
            // for the current scope.
            if (s.mod !is null)
            {
                Module.addDeferredSemantic2(s);     // https://issues.dlang.org/show_bug.cgi?id=14666
                sc.insert(s);

                foreach (aliasdecl; s.aliasdecls)
                {
                    sc.insert(aliasdecl);
                }
            }
        }
        result = imps;
    }

    mixin VisitStatement!void visit;
    visit.VisitStatement(s);
    return result;
}

/**
 * Run semantic on `throw <exp>`.
 *
 * Params:
 *   loc = location of the `throw`
 *   exp = value to be thrown
 *   sc  = enclosing scope
 *
 * Returns: true if the `throw` is valid, or false if an error was found
 */
public bool throwSemantic(const ref Loc loc, ref Expression exp, Scope* sc)
{
    if (!global.params.useExceptions)
    {
        version (IN_GCC)
            loc.error("cannot use `throw` statements with `-fno-exceptions`");
        else
            loc.error("cannot use `throw` statements with %s", global.params.betterC ? "-betterC".ptr : "-nothrow".ptr);
        return false;
    }

    if (!ClassDeclaration.throwable)
    {
        loc.error("cannot use `throw` statements because `object.Throwable` was not declared");
        return false;
    }

    if (auto ne = exp.isNewExp())
    {
        ne.thrownew = true;
    }

    exp = exp.expressionSemantic(sc);
    exp = resolveProperties(sc, exp);
    exp = checkGC(sc, exp);
    if (exp.isErrorExp())
        return false;
    if (!exp.type.isNaked())
    {
        // @@@DEPRECATED_2.112@@@
        // Deprecated in 2.102, change into an error & return false in 2.112
        exp.loc.deprecation("cannot throw object of qualified type `%s`", exp.type.toChars());
        //return false;
    }
    checkThrowEscape(*sc, exp, false);

    ClassDeclaration cd = exp.type.toBasetype().isClassHandle();
    if (!cd || ((cd != ClassDeclaration.throwable) && !ClassDeclaration.throwable.isBaseOf(cd, null)))
    {
        loc.error("can only throw class objects derived from `Throwable`, not type `%s`", exp.type.toChars());
        return false;
    }
    return true;
}

private extern(D) Expression applyOpApply(ForeachStatement fs, Expression flde,
            Type tab, Scope* sc2, Dsymbol sapply)
{
    version (none)
    {
        if (sc2.useDIP1000 == FeatureState.enabled)
        {
            message(loc, "To enforce `@safe`, the compiler allocates a closure unless `opApply()` uses `scope`");
        }
        flde.isFuncExp().fd.tookAddressOf = 1;
    }
    else
    {
        if (sc2.useDIP1000 == FeatureState.enabled)
            ++flde.isFuncExp().fd.tookAddressOf;  // allocate a closure unless the opApply() uses 'scope'
    }
    assert(tab.ty == Tstruct || tab.ty == Tclass);
    assert(sapply);
    /* Call:
     *  aggr.apply(flde)
     */
    Expression ec;
    ec = new DotIdExp(fs.loc, fs.aggr, sapply.ident);
    ec = new CallExp(fs.loc, ec, flde);
    ec = ec.expressionSemantic(sc2);
    if (ec.isErrorExp())
        return null;
    if (ec.type != Type.tint32)
    {
        error(fs.loc, "`opApply()` function for `%s` must return an `int`", tab.toChars());
        return null;
    }
    return ec;
}

private extern(D) Expression applyDelegate(ForeachStatement fs, Expression flde,
                                                  Type tab, Scope* sc2)
{
    Expression ec;
    /* Call:
     *      aggr(flde)
     */
    if (auto de = fs.aggr.isDelegateExp())
    if (de.func.isNested() &&
        !de.func.needThis())
    {
        // https://issues.dlang.org/show_bug.cgi?id=3560
        fs.aggr = de.e1;
    }
    ec = new CallExp(fs.loc, fs.aggr, flde);
    ec = ec.expressionSemantic(sc2);
    if (ec.op == EXP.error)
        return null;
    if (ec.type != Type.tint32)
    {
        error(fs.loc, "`opApply()` function for `%s` must return an `int`", tab.toChars());
        return null;
    }
    return ec;
}

private extern(D) Expression applyArray(ForeachStatement fs, Expression flde,
                                               Type tab, Scope* sc2, Type tn, Type tnv)
{
    Expression ec;
    const dim = fs.parameters.length;
    const loc = fs.loc;
    /* Call:
     *      _aApply(aggr, flde)
     */
    static immutable fntab =
    [
     "cc", "cw", "cd",
     "wc", "cc", "wd",
     "dc", "dw", "dd"
    ];

    const(size_t) BUFFER_LEN = 7 + 1 + 2 + dim.sizeof * 3 + 1;
    char[BUFFER_LEN] fdname;
    int flag;

    switch (tn.ty)
    {
        case Tchar:     flag = 0;   break;
        case Twchar:    flag = 3;   break;
        case Tdchar:    flag = 6;   break;
        default:
            assert(0);
    }
    switch (tnv.ty)
    {
        case Tchar:     flag += 0;  break;
        case Twchar:    flag += 1;  break;
        case Tdchar:    flag += 2;  break;
        default:
            assert(0);
    }
    const(char)* r = (fs.op == TOK.foreach_reverse_) ? "R" : "";
    int j = snprintf(fdname.ptr, BUFFER_LEN,  "_aApply%s%.*s%llu", r, 2, fntab[flag].ptr, cast(ulong)dim);
    assert(j < BUFFER_LEN);

    FuncDeclaration fdapply;
    TypeDelegate dgty;
    auto params = new Parameters();
    params.push(new Parameter(Loc.initial, STC.in_, tn.arrayOf(), null, null, null));
    auto dgparams = new Parameters();
    dgparams.push(new Parameter(Loc.initial, 0, Type.tvoidptr, null, null, null));
    if (dim == 2)
        dgparams.push(new Parameter(Loc.initial, 0, Type.tvoidptr, null, null, null));
    dgty = new TypeDelegate(new TypeFunction(ParameterList(dgparams), Type.tint32, LINK.d));
    params.push(new Parameter(Loc.initial, 0, dgty, null, null, null));
    fdapply = FuncDeclaration.genCfunc(params, Type.tint32, fdname.ptr);

    if (tab.isTypeSArray())
        fs.aggr = fs.aggr.castTo(sc2, tn.arrayOf());
    // paint delegate argument to the type runtime expects
    Expression fexp = flde;
    if (!dgty.equals(flde.type))
    {
        fexp = new CastExp(loc, flde, flde.type);
        fexp.type = dgty;
    }
    ec = new VarExp(Loc.initial, fdapply, false);
    ec = new CallExp(loc, ec, fs.aggr, fexp);
    ec.type = Type.tint32; // don't run semantic() on ec
    return ec;
}

private extern(D) Expression applyAssocArray(ForeachStatement fs, Expression flde, Type tab)
{
    auto taa = tab.isTypeAArray();
    Expression ec;
    const dim = fs.parameters.length;
    // Check types
    Parameter p = (*fs.parameters)[0];
    bool isRef = (p.storageClass & STC.ref_) != 0;
    Type ta = p.type;
    if (dim == 2)
    {
        Type ti = (isRef ? taa.index.addMod(MODFlags.const_) : taa.index);
        if (isRef ? !ti.constConv(ta) : !ti.implicitConvTo(ta))
        {
            error(fs.loc, "`foreach`: index must be type `%s`, not `%s`",
                     ti.toChars(), ta.toChars());
            return null;
        }
        p = (*fs.parameters)[1];
        isRef = (p.storageClass & STC.ref_) != 0;
        ta = p.type;
    }
    Type taav = taa.nextOf();
    if (isRef ? !taav.constConv(ta) : !taav.implicitConvTo(ta))
    {
        error(fs.loc, "`foreach`: value must be type `%s`, not `%s`",
                 taav.toChars(), ta.toChars());
        return null;
    }

    /* Call:
     *  extern(C) int _aaApply(void*, in size_t, int delegate(void*))
     *      _aaApply(aggr, keysize, flde)
     *
     *  extern(C) int _aaApply2(void*, in size_t, int delegate(void*, void*))
     *      _aaApply2(aggr, keysize, flde)
     */
    __gshared FuncDeclaration* fdapply = [null, null];
    __gshared TypeDelegate* fldeTy = [null, null];
    ubyte i = (dim == 2 ? 1 : 0);
    if (!fdapply[i])
    {
        auto params = new Parameters();
        params.push(new Parameter(Loc.initial, 0, Type.tvoid.pointerTo(), null, null, null));
        params.push(new Parameter(Loc.initial, STC.const_, Type.tsize_t, null, null, null));
        auto dgparams = new Parameters();
        dgparams.push(new Parameter(Loc.initial, 0, Type.tvoidptr, null, null, null));
        if (dim == 2)
            dgparams.push(new Parameter(Loc.initial, 0, Type.tvoidptr, null, null, null));
        fldeTy[i] = new TypeDelegate(new TypeFunction(ParameterList(dgparams), Type.tint32, LINK.d));
        params.push(new Parameter(Loc.initial, 0, fldeTy[i], null, null, null));
        fdapply[i] = FuncDeclaration.genCfunc(params, Type.tint32, i ? Id._aaApply2 : Id._aaApply);
    }

    auto exps = new Expressions();
    exps.push(fs.aggr);
    auto keysize = taa.index.size();
    if (keysize == SIZE_INVALID)
        return null;
    assert(keysize < keysize.max - target.ptrsize);
    keysize = (keysize + (target.ptrsize - 1)) & ~(target.ptrsize - 1);
    // paint delegate argument to the type runtime expects
    Expression fexp = flde;
    if (!fldeTy[i].equals(flde.type))
    {
        fexp = new CastExp(fs.loc, flde, flde.type);
        fexp.type = fldeTy[i];
    }
    exps.push(new IntegerExp(Loc.initial, keysize, Type.tsize_t));
    exps.push(fexp);
    ec = new VarExp(Loc.initial, fdapply[i], false);
    ec = new CallExp(fs.loc, ec, exps);
    ec.type = Type.tint32; // don't run semantic() on ec
    return ec;
}

private extern(D) Statement loopReturn(Expression e, Statements* cases, const ref Loc loc)
{
    if (!cases.length)
    {
        // Easy case, a clean exit from the loop
        e = new CastExp(loc, e, Type.tvoid); // https://issues.dlang.org/show_bug.cgi?id=13899
        return new ExpStatement(loc, e);
    }
    // Construct a switch statement around the return value
    // of the apply function.
    Statement s;
    auto a = new Statements();

    // default: break; takes care of cases 0 and 1
    s = new BreakStatement(Loc.initial, null);
    s = new DefaultStatement(Loc.initial, s);
    a.push(s);

    // cases 2...
    foreach (i, c; *cases)
    {
        s = new CaseStatement(Loc.initial, new IntegerExp(i + 2), c);
        a.push(s);
    }

    s = new CompoundStatement(loc, a);
    return new SwitchStatement(loc, null, e, s, false, loc);
}

/*************************************
 * Turn foreach body into the function literal:
 *  int delegate(ref T param) { body }
 * Params:
 *  sc = context
 *  fs = ForeachStatement
 *  tfld = type of function literal to be created (type of opApply() function if any), can be null
 * Returns:
 *  Function literal created, as an expression
 *  null if error.
 */
private FuncExp foreachBodyToFunction(Scope* sc, ForeachStatement fs, TypeFunction tfld)
{
    auto params = new Parameters();
    foreach (i, p; *fs.parameters)
    {
        StorageClass stc = STC.ref_ | (p.storageClass & STC.scope_);
        Identifier id;

        p.type = p.type.typeSemantic(fs.loc, sc);
        p.type = p.type.addStorageClass(p.storageClass);
        if (tfld)
        {
            Parameter param = tfld.parameterList[i];
            //printf("\tparam = %s%s\n", (param.storageClass&STC.ref_?"ref ":"").ptr, param.ident.toChars());
            stc = (param.storageClass & STC.ref_) | (p.storageClass & STC.scope_);
            if ((p.storageClass & STC.ref_) != (param.storageClass & STC.ref_))
            {
                if (!(param.storageClass & STC.ref_))
                {
                    error(fs.loc, "`foreach`: cannot make `%s` `ref`", p.ident.toChars());
                    return null;
                }
                goto LcopyArg;
            }
            id = p.ident; // argument copy is not need.
        }
        else if (p.storageClass & STC.ref_)
        {
            // default delegate parameters are marked as ref, then
            // argument copy is not need.
            id = p.ident;
        }
        else
        {
            // Make a copy of the ref argument so it isn't
            // a reference.
        LcopyArg:
            id = Identifier.generateId("__applyArg", cast(int)i);

            Initializer ie = new ExpInitializer(fs.loc, new IdentifierExp(fs.loc, id));
            auto v = new VarDeclaration(fs.loc, p.type, p.ident, ie);
            v.storage_class |= STC.temp | (stc & STC.scope_);
            Statement s = new ExpStatement(fs.loc, v);
            fs._body = new CompoundStatement(fs.loc, s, fs._body);
        }
        params.push(new Parameter(fs.loc, stc, p.type, id, null, null));
    }
    // https://issues.dlang.org/show_bug.cgi?id=13840
    // Throwable nested function inside nothrow function is acceptable.
    StorageClass stc = mergeFuncAttrs(STC.safe | STC.pure_ | STC.nogc, fs.func);
    auto tf = new TypeFunction(ParameterList(params), Type.tint32, LINK.d, stc);
    fs.cases = new Statements();
    fs.gotos = new ScopeStatements();
    auto fld = new FuncLiteralDeclaration(fs.loc, fs.endloc, tf, TOK.delegate_, fs);
    fld.fbody = fs._body;
    Expression flde = new FuncExp(fs.loc, fld);
    flde = flde.expressionSemantic(sc);
    fld.tookAddressOf = 0;
    if (flde.op == EXP.error)
        return null;
    return flde.isFuncExp();
}


void catchSemantic(Catch c, Scope* sc)
{
    //printf("Catch::semantic(%s)\n", ident.toChars());

    if (sc.os && sc.os.tok != TOK.onScopeFailure)
    {
        // If enclosing is scope(success) or scope(exit), this will be placed in finally block.
        error(c.loc, "cannot put `catch` statement inside `%s`", Token.toChars(sc.os.tok));
        c.errors = true;
    }
    if (sc.tf)
    {
        /* This is because the _d_local_unwind() gets the stack munged
         * up on this. The workaround is to place any try-catches into
         * a separate function, and call that.
         * To fix, have the compiler automatically convert the finally
         * body into a nested function.
         */
        error(c.loc, "cannot put `catch` statement inside `finally` block");
        c.errors = true;
    }

    auto sym = new ScopeDsymbol();
    sym.parent = sc.scopesym;
    sc = sc.push(sym);

    if (!c.type)
    {
        error(c.loc, "`catch` statement without an exception specification is deprecated");
        errorSupplemental(c.loc, "use `catch(Throwable)` for old behavior");
        c.errors = true;

        // reference .object.Throwable
        c.type = getThrowable();
    }
    else if (!c.type.isNaked() && !c.type.isConst())
    {
        // @@@DEPRECATED_2.115@@@
        // Deprecated in 2.105, change into an error & uncomment assign in 2.115
        deprecation(c.loc, "can only catch mutable or const qualified types, not `%s`", c.type.toChars());
        //c.errors = true;
    }
    c.type = c.type.typeSemantic(c.loc, sc);
    if (c.type == Type.terror)
    {
        c.errors = true;
        sc.pop();
        return;
    }

    StorageClass stc;
    auto cd = c.type.toBasetype().isClassHandle();
    if (!cd)
    {
        error(c.loc, "can only catch class objects, not `%s`", c.type.toChars());
        c.errors = true;
    }
    else if (cd.isCPPclass())
    {
        if (!target.cpp.exceptions)
        {
            error(c.loc, "catching C++ class objects not supported for this target");
            c.errors = true;
        }
        if (!c.internalCatch)
        {
            if (sc.setUnsafe(false, c.loc, "catching C++ class objects"))
                c.errors = true;
        }
    }
    else if (cd != ClassDeclaration.throwable && !ClassDeclaration.throwable.isBaseOf(cd, null))
    {
        error(c.loc, "can only catch class objects derived from `Throwable`, not `%s`", c.type.toChars());
        c.errors = true;
    }
    else if (!c.internalCatch && ClassDeclaration.exception &&
            cd != ClassDeclaration.exception && !ClassDeclaration.exception.isBaseOf(cd, null) &&
            sc.setUnsafe(false, c.loc,
                "catching class objects derived from `%s` instead of `Exception`", c.type))
    {
        c.errors = true;
    }
    else if (sc.previews.dip1008)
    {
        stc |= STC.scope_;
    }

    // DIP1008 requires destruction of the Throwable, even if the user didn't specify an identifier
    auto ident = c.ident;
    if (!ident && sc.previews.dip1008)
        ident = Identifier.generateAnonymousId("var");

    if (ident)
    {
        c.var = new VarDeclaration(c.loc, c.type, ident, null, stc);
        c.var.iscatchvar = true;
        c.var.dsymbolSemantic(sc);
        sc.insert(c.var);

        if (sc.previews.dip1008 && stc & STC.scope_)
        {
            /* Add a destructor for c.var
             * try { handler } finally { if (!__ctfe) _d_delThrowable(var); }
             */
            assert(!c.var.edtor);           // ensure we didn't create one in callScopeDtor()

            Loc loc = c.loc;
            Expression e = new VarExp(loc, c.var);
            e = new CallExp(loc, new IdentifierExp(loc, Id._d_delThrowable), e);

            Expression ec = new IdentifierExp(loc, Id.ctfe);
            ec = new NotExp(loc, ec);
            Statement s = new IfStatement(loc, null, ec, new ExpStatement(loc, e), null, loc);
            c.handler = new TryFinallyStatement(loc, c.handler, s);
        }

    }
    c.handler = c.handler.statementSemantic(sc);
    if (c.handler && c.handler.isErrorStatement())
        c.errors = true;

    sc.pop();
}

Statement semanticNoScope(Statement s, Scope* sc)
{
    //printf("Statement::semanticNoScope() %s\n", toChars());
    if (!s.isCompoundStatement() && !s.isScopeStatement())
    {
        s = new CompoundStatement(s.loc, s); // so scopeCode() gets called
    }
    s = s.statementSemantic(sc);
    return s;
}

// Same as semanticNoScope(), but do create a new scope
private Statement semanticScope(Statement s, Scope* sc, Statement sbreak, Statement scontinue, Statement tryBody)
{
    auto sym = new ScopeDsymbol();
    sym.parent = sc.scopesym;
    Scope* scd = sc.push(sym);
    if (sbreak)
        scd.sbreak = sbreak;
    if (scontinue)
        scd.scontinue = scontinue;
    if (tryBody)
        scd.tryBody = tryBody;
    s = s.semanticNoScope(scd);
    scd.pop();
    return s;
}


/****************************************
 * If `statement` has code that needs to run in a finally clause
 * at the end of the current scope, return that code in the form of
 * a Statement.
 * Params:
 *     statement = the statement
 *     sc = context
 *     sentry     = set to code executed upon entry to the scope
 *     sexception = set to code executed upon exit from the scope via exception
 *     sfinally   = set to code executed in finally block
 * Returns:
 *    code to be run in the finally clause
 */
Statement scopeCode(Statement statement, Scope* sc, out Statement sentry, out Statement sexception, out Statement sfinally)
{
    if (auto es = statement.isExpStatement())
    {
        if (es.exp && es.exp.isDeclarationExp())
        {
            auto de = es.exp.isDeclarationExp();
            auto v = de.declaration.isVarDeclaration();
            if (v && !v.isDataseg())
            {
                if (v.needsScopeDtor())
                {
                    sfinally = new DtorExpStatement(es.loc, v.edtor, v);
                    v.storage_class |= STC.nodtor; // don't add in dtor again
                }
            }
        }
        return es;

    }
    else if (auto sgs = statement.isScopeGuardStatement())
    {
        Statement s = new PeelStatement(sgs.statement);

        switch (sgs.tok)
        {
        case TOK.onScopeExit:
            sfinally = s;
            break;

        case TOK.onScopeFailure:
            sexception = s;
            break;

        case TOK.onScopeSuccess:
            {
                /* Create:
                 *  sentry:   bool x = false;
                 *  sexception:    x = true;
                 *  sfinally: if (!x) statement;
                 */
                auto v = copyToTemp(0, "__os", IntegerExp.createBool(false));
                v.dsymbolSemantic(sc);
                sentry = new ExpStatement(statement.loc, v);

                Expression e = IntegerExp.createBool(true);
                e = new AssignExp(Loc.initial, new VarExp(Loc.initial, v), e);
                sexception = new ExpStatement(Loc.initial, e);

                e = new VarExp(Loc.initial, v);
                e = new NotExp(Loc.initial, e);
                sfinally = new IfStatement(Loc.initial, null, e, s, null, Loc.initial);

                break;
            }
        default:
            assert(0);
        }
        return null;
    }
    else if (auto ls = statement.isLabelStatement())
    {
        if (ls.statement)
            ls.statement = ls.statement.scopeCode(sc, sentry, sexception, sfinally);
        return ls;
    }

    return statement;
}

/*******************
 * Type check and unroll `foreach` over an expression tuple as well
 * as `static foreach` statements and `static foreach`
 * declarations. For `static foreach` statements and `static
 * foreach` declarations, the visitor interface is used (and the
 * result is written into the `result` field.) For `static
 * foreach` declarations, the resulting Dsymbols* are returned
 * directly.
 *
 * The unrolled body is wrapped into a
 *  - UnrolledLoopStatement, for `foreach` over an expression tuple.
 *  - ForwardingStatement, for `static foreach` statements.
 *  - ForwardingAttribDeclaration, for `static foreach` declarations.
 *
 * `static foreach` variables are declared as `STC.local`, such
 * that they are inserted into the local symbol tables of the
 * forwarding constructs instead of forwarded. For `static
 * foreach` with multiple foreach loop variables whose aggregate
 * has been lowered into a sequence of tuples, this function
 * expands the tuples into multiple `STC.local` `static foreach`
 * variables.
 */
public auto makeTupleForeach(Scope* sc, bool isStatic, bool isDecl, ForeachStatement fs, Dsymbols* dbody, bool needExpansion)
{
    // Voldemort return type
    union U
    {
        Statement statement;
        Dsymbols* decl;
    }

    U result;

    auto returnEarly()
    {
        if (isDecl)
            result.decl = null;
        else
            result.statement = new ErrorStatement();
        return result;
    }

    auto loc = fs.loc;
    size_t dim = fs.parameters.length;
    const bool skipCheck = isStatic && needExpansion;
    if (!skipCheck && (dim < 1 || dim > 2))
    {
        error(fs.loc, "only one (value) or two (key,value) arguments allowed for sequence `foreach`");
        return returnEarly();
    }

    Type paramtype = (*fs.parameters)[dim - 1].type;
    if (paramtype)
    {
        paramtype = paramtype.typeSemantic(loc, sc);
        if (paramtype.ty == Terror)
        {
            return returnEarly();
        }
    }

    Type tab = fs.aggr.type.toBasetype();
    TypeTuple tuple = tab.isTypeTuple();

    Statements* statements;
    Dsymbols* declarations;
    if (isDecl)
        declarations = new Dsymbols();
    else
        statements = new Statements();

    //printf("aggr: op = %d, %s\n", fs.aggr.op, fs.aggr.toChars());
    size_t n;
    TupleExp te = null;
    if (auto ate =  fs.aggr.isTupleExp()) // expression tuple
    {
        te = ate;
        n = te.exps.length;
    }
    else if (fs.aggr.isTypeExp()) // type tuple
    {
        n = Parameter.dim(tuple.arguments);
    }
    else
        assert(0);
    foreach (j; 0 .. n)
    {
        size_t k = (fs.op == TOK.foreach_) ? j : n - 1 - j;
        Expression e = null;
        Type t = null;
        if (te)
            e = (*te.exps)[k];
        else
            t = Parameter.getNth(tuple.arguments, k).type;
        Parameter p = (*fs.parameters)[0];

        Statements* stmts;
        Dsymbols* decls;
        if (isDecl)
            decls = new Dsymbols();
        else
            stmts = new Statements();

        const bool skip = isStatic && needExpansion;
        if (!skip && dim == 2)
        {
            // Declare key
            if (p.isReference() || p.isLazy())
            {
                error(fs.loc, "no storage class for key `%s`", p.ident.toChars());
                return returnEarly();
            }

            if (isStatic)
            {
                if (!p.type)
                {
                    p.type = Type.tsize_t;
                }
            }
            p.type = p.type.typeSemantic(loc, sc);

            if (!p.type.isIntegral())
            {
                error(fs.loc, "foreach: key cannot be of non-integral type `%s`",
                         p.type.toChars());
                return returnEarly();
            }

            const length = te ? te.exps.length : tuple.arguments.length;
            IntRange dimrange = IntRange(SignExtendedNumber(length))._cast(Type.tsize_t);
            // https://issues.dlang.org/show_bug.cgi?id=12504
            dimrange.imax = SignExtendedNumber(dimrange.imax.value-1);
            if (!IntRange.fromType(p.type).contains(dimrange))
            {
                error(fs.loc, "index type `%s` cannot cover index range 0..%llu",
                         p.type.toChars(), cast(ulong)length);
                return returnEarly();
            }
            Initializer ie = new ExpInitializer(Loc.initial, new IntegerExp(k));
            auto var = new VarDeclaration(loc, p.type, p.ident, ie);
            var.storage_class |= STC.foreach_ | STC.manifest;
            if (isStatic)
                var.storage_class |= STC.local;

            if (isDecl)
                decls.push(var);
            else
                stmts.push(new ExpStatement(loc, var));

            p = (*fs.parameters)[1]; // value
        }
        /***********************
         * Declares a unrolled `foreach` loop variable or a `static foreach` variable.
         *
         * Params:
         *     storageClass = The storage class of the variable.
         *     type = The declared type of the variable.
         *     ident = The name of the variable.
         *     e = The initializer of the variable (i.e. the current element of the looped over aggregate).
         *     t = The type of the initializer.
         * Returns:
         *     `true` iff the declaration was successful.
         */
        bool declareVariable(StorageClass storageClass, Type type, Identifier ident, Expression e, Type t)
        {
            if (storageClass & (STC.out_ | STC.lazy_) ||
                storageClass & STC.ref_ && !te)
            {
                error(fs.loc, "no storage class for value `%s`", ident.toChars());
                return false;
            }
            Declaration var;
            if (e)
            {
                Type tb = e.type.toBasetype();
                Dsymbol ds = null;
                if (!(storageClass & STC.manifest))
                {
                    if (isStatic || tb.ty == Tfunction || storageClass & STC.alias_)
                    {
                        if (auto ve = e.isVarExp())
                            ds = ve.var;
                        else if (auto dve = e.isDotVarExp())
                            ds = dve.var;
                    }
                    if (auto te = e.isTemplateExp())
                        ds = te.td;
                    else if (auto se = e.isScopeExp())
                        ds = se.sds;
                    else if (auto fe = e.isFuncExp())
                        ds = fe.td ? fe.td : fe.fd;
                    else if (auto oe = e.isOverExp())
                        ds = oe.vars;
                }
                else if (storageClass & STC.alias_)
                {
                    error(fs.loc, "`foreach` loop variable cannot be both `enum` and `alias`");
                    return false;
                }

                if (ds)
                {
                    var = new AliasDeclaration(loc, ident, ds);
                    if (storageClass & STC.ref_)
                    {
                        error(fs.loc, "symbol `%s` cannot be `ref`", ds.toChars());
                        return false;
                    }
                    if (paramtype)
                    {
                        error(fs.loc, "cannot specify element type for symbol `%s`", ds.toChars());
                        return false;
                    }
                }
                else if (e.op == EXP.type)
                {
                    var = new AliasDeclaration(loc, ident, e.type);
                    if (paramtype)
                    {
                        error(fs.loc, "cannot specify element type for type `%s`", e.type.toChars());
                        return false;
                    }
                }
                else
                {
                    e = resolveProperties(sc, e);
                    Initializer ie = new ExpInitializer(Loc.initial, e);
                    auto v = new VarDeclaration(loc, type, ident, ie, storageClass);
                    v.storage_class |= STC.foreach_;
                    if (storageClass & STC.ref_)
                        v.storage_class |= STC.ref_;
                    if (isStatic || storageClass&STC.manifest || e.isConst() ||
                        e.op == EXP.string_ ||
                        e.op == EXP.structLiteral ||
                        e.op == EXP.arrayLiteral)
                    {
                        if (v.storage_class & STC.ref_)
                        {
                            if (!isStatic)
                            {
                                error(fs.loc, "constant value `%s` cannot be `ref`", toChars(ie));
                            }
                            else
                            {
                                if (!needExpansion)
                                {
                                    error(fs.loc, "constant value `%s` cannot be `ref`", toChars(ie));
                                }
                                else
                                {
                                    error(fs.loc, "constant value `%s` cannot be `ref`", ident.toChars());
                                }
                            }
                            return false;
                        }
                        else
                            v.storage_class |= STC.manifest;
                    }
                    var = v;
                }
            }
            else
            {
                var = new AliasDeclaration(loc, ident, t);
                if (paramtype)
                {
                    error(fs.loc, "cannot specify element type for symbol `%s`", fs.toChars());
                    return false;
                }
            }
            if (isStatic)
            {
                var.storage_class |= STC.local;
            }

            if (isDecl)
                decls.push(var);
            else
                stmts.push(new ExpStatement(loc, var));
            return true;
        }

        if (!isStatic)
        {
            // Declare value
            if (!declareVariable(p.storageClass, p.type, p.ident, e, t))
            {
                return returnEarly();
            }
        }
        else
        {
            if (!needExpansion)
            {
                // Declare value
                if (!declareVariable(p.storageClass, p.type, p.ident, e, t))
                {
                    return returnEarly();
                }
            }
            else
            {   // expand tuples into multiple `static foreach` variables.
                assert(e && !t);
                auto ident = Identifier.generateId("__value");
                declareVariable(0, e.type, ident, e, null);
                import dmd.cond: StaticForeach;
                auto field = Identifier.idPool(StaticForeach.tupleFieldName.ptr,StaticForeach.tupleFieldName.length);
                Expression access = new DotIdExp(loc, e, field);
                access = expressionSemantic(access, sc);
                access = access.optimize(WANTvalue);
                if (!tuple) return returnEarly();
                //printf("%s\n",tuple.toChars());
                foreach (l; 0 .. dim)
                {
                    auto cp = (*fs.parameters)[l];
                    Expression init_ = new IndexExp(loc, access, new IntegerExp(loc, l, Type.tsize_t));
                    init_ = init_.expressionSemantic(sc);
                    assert(init_.type);
                    declareVariable(p.storageClass, init_.type, cp.ident, init_, null);
                }
            }
        }

        Statement s;
        Dsymbol d;
        if (isDecl)
            decls.append(Dsymbol.arraySyntaxCopy(dbody));
        else
        {
            stmts.push(fs._body.syntaxCopy());
            s = new CompoundStatement(loc, stmts);
        }

        if (!isStatic)
        {
            s = new ScopeStatement(loc, s, fs.endloc);
        }
        else if (isDecl)
        {
            import dmd.attrib: ForwardingAttribDeclaration;
            d = new ForwardingAttribDeclaration(decls);
        }
        else
        {
            s = new ForwardingStatement(loc, s);
        }

        if (isDecl)
            declarations.push(d);
        else
            statements.push(s);
    }

    if (!isStatic)
    {
        Statement res = new UnrolledLoopStatement(loc, statements);
        if (LabelStatement ls = checkLabeledLoop(sc, fs))
            ls.gotoTarget = res;
        if (te && te.e0)
            res = new CompoundStatement(loc, new ExpStatement(te.e0.loc, te.e0), res);
        result.statement = res;
    }
    else if (isDecl)
        result.decl = declarations;
    else
        result.statement = new CompoundStatement(loc, statements);

    return result;
}

/*********************************
 * Flatten out the scope by presenting `statement`
 * as an array of statements.
 * Params:
 *     statement = the statement to flatten
 *     sc = context
 * Returns:
 *     The array of `Statements`, or `null` if no flattening necessary
 */
private Statements* flatten(Statement statement, Scope* sc)
{
    static auto errorStatements()
    {
        auto a = new Statements();
        a.push(new ErrorStatement());
        return a;
    }


    /*compound and expression statements have classes that inherit from them with the same
     *flattening behavior, so the isXXX methods won't work
     */
    switch(statement.stmt)
    {
        case STMT.Compound:
        case STMT.CompoundDeclaration:
            return (cast(CompoundStatement)statement).statements;

        case STMT.Exp:
        case STMT.DtorExp:
            auto es = cast(ExpStatement)statement;
            /* https://issues.dlang.org/show_bug.cgi?id=14243
             * expand template mixin in statement scope
             * to handle variable destructors.
             */
            if (!es.exp || !es.exp.isDeclarationExp())
                return null;

            Dsymbol d = es.exp.isDeclarationExp().declaration;
            auto tm = d.isTemplateMixin();
            if (!tm)
                return null;

            Expression e = es.exp.expressionSemantic(sc);
            if (e.op == EXP.error || tm.errors)
                return errorStatements();
            assert(tm.members);

            Statement s = toStatement(tm);
            version (none)
            {
                OutBuffer buf;
                buf.doindent = 1;
                HdrGenState hgs;
                hgs.hdrgen = true;
                toCBuffer(s, &buf, &hgs);
                printf("tm ==> s = %s\n", buf.peekChars());
            }
            auto a = new Statements();
            a.push(s);
            return a;

        case STMT.Forwarding:
            /***********************
             * ForwardingStatements are distributed over the flattened
             * sequence of statements. This prevents flattening to be
             * "blocked" by a ForwardingStatement and is necessary, for
             * example, to support generating scope guards with `static
             * foreach`:
             *
             *     static foreach(i; 0 .. 10) scope(exit) writeln(i);
             *     writeln("this is printed first");
             *     // then, it prints 10, 9, 8, 7, ...
             */
            auto fs = statement.isForwardingStatement();
            if (!fs.statement)
            {
                return null;
            }
            sc = sc.push(fs.sym);
            auto a = fs.statement.flatten(sc);
            sc = sc.pop();
            if (!a)
            {
                return a;
            }
            auto b = new Statements(a.length);
            foreach (i, s; *a)
            {
                (*b)[i] = s ? new ForwardingStatement(s.loc, fs.sym, s) : null;
            }
            return b;

        case STMT.Conditional:
            auto cs = statement.isConditionalStatement();
            Statement s;

            //printf("ConditionalStatement::flatten()\n");
            if (cs.condition.include(sc))
            {
                DebugCondition dc = cs.condition.isDebugCondition();
                if (dc)
                {
                    s = new DebugStatement(cs.loc, cs.ifbody);
                    debugThrowWalker(cs.ifbody);
                }
                else
                    s = cs.ifbody;
            }
            else
                s = cs.elsebody;

            auto a = new Statements();
            a.push(s);
            return a;

        case STMT.StaticForeach:
            auto sfs = statement.isStaticForeachStatement();
            sfs.sfe.prepare(sc);
            if (sfs.sfe.ready())
            {
                Statement s = makeTupleForeach(sc, true, false, sfs.sfe.aggrfe, null, sfs.sfe.needExpansion).statement;
                auto result = s.flatten(sc);
                if (result)
                {
                    return result;
                }
                result = new Statements();
                result.push(s);
                return result;
            }
            else
                return errorStatements();

        case STMT.Debug:
            auto ds = statement.isDebugStatement();
            Statements* a = ds.statement ? ds.statement.flatten(sc) : null;
            if (!a)
                return null;

            foreach (ref s; *a)
            {
                s = new DebugStatement(ds.loc, s);
            }
            return a;

        case STMT.Label:
            auto ls = statement.isLabelStatement();
            if (!ls.statement)
                return null;

            Statements* a = null;
            a = ls.statement.flatten(sc);
            if (!a)
                return null;

            if (!a.length)
            {
                a.push(new ExpStatement(ls.loc, cast(Expression)null));
            }

            // reuse 'this' LabelStatement
            ls.statement = (*a)[0];
            (*a)[0] = ls;
            return a;

        case STMT.Mixin:
            auto cs = statement.isMixinStatement();


            OutBuffer buf;
            if (expressionsToString(buf, sc, cs.exps, cs.loc, null, true))
                return errorStatements();

            const errors = global.errors;
            const len = buf.length;
            buf.writeByte(0);
            const str = buf.extractSlice()[0 .. len];
            const bool doUnittests = global.params.parsingUnittestsRequired();
            auto loc = adjustLocForMixin(str, cs.loc, global.params.mixinOut);
            scope p = new Parser!ASTCodegen(loc, sc._module, str, false, global.errorSink, &global.compileEnv, doUnittests);
            p.nextToken();

            auto a = new Statements();
            while (p.token.value != TOK.endOfFile)
            {
                Statement s = p.parseStatement(ParseStatementFlags.curlyScope);
                if (!s || global.errors != errors)
                    return errorStatements();
                a.push(s);
            }
            return a;
        default:
            return null;
    }
}

/***********************************************************
 * Convert TemplateMixin members (which are Dsymbols) to Statements.
 * Params:
 *    s = the symbol to convert to a Statement
 * Returns:
 *    s redone as a Statement
 */
private Statement toStatement(Dsymbol s)
{
    Statement result;

    if (auto tm = s.isTemplateMixin())
    {
        auto a = new Statements();
        foreach (m; *tm.members)
        {
            if (Statement sx = toStatement(m))
                a.push(sx);
        }
        result = new CompoundStatement(tm.loc, a);
    }
    else if (s.isVarDeclaration()       ||
             s.isAggregateDeclaration() ||
             s.isFuncDeclaration()      ||
             s.isEnumDeclaration()      ||
             s.isAliasDeclaration()     ||
             s.isTemplateDeclaration())
    {
        /* Perhaps replace the above with isScopeDsymbol() || isDeclaration()
         */
        /* An actual declaration symbol will be converted to DeclarationExp
         * with ExpStatement.
         */
        auto de = new DeclarationExp(s.loc, s);
        de.type = Type.tvoid; // avoid repeated semantic
        result = new ExpStatement(s.loc, de);
    }
    else if (auto d = s.isAttribDeclaration())
    {
        /* All attributes have been already picked by the semantic analysis of
         * 'bottom' declarations (function, struct, class, etc).
         * So we don't have to copy them.
         */
        if (Dsymbols* a = d.include(null))
        {
            auto statements = new Statements();
            foreach (sx; *a)
            {
                statements.push(toStatement(sx));
            }
            result = new CompoundStatement(d.loc, statements);
        }
    }
    else if (s.isStaticAssert() ||
             s.isImport())
    {
        /* Ignore as they are not Statements
         */
    }
    else
    {
        .error(Loc.initial, "internal compiler error: cannot mixin %s `%s`\n", s.kind(), s.toChars());
        result = new ErrorStatement();
    }

    return result;
}

/**
Marks all occurring ThrowStatements as internalThrows.
This is intended to be called from a DebugStatement as it allows
to mark all its nodes as nothrow.

Params:
    s = AST Node to traverse
*/
private void debugThrowWalker(Statement s)
{

    extern(C++) final class DebugWalker : SemanticTimeTransitiveVisitor
    {
        alias visit = SemanticTimeTransitiveVisitor.visit;
    public:

        override void visit(ThrowStatement s)
        {
            s.internalThrow = true;
        }

        override void visit(CallExp s)
        {
            s.inDebugStatement = true;
        }
    }

    scope walker = new DebugWalker();
    s.accept(walker);
}

/************************************
 * Check for skipped variable declarations.
 * Params:
 *      ss = statement to check
 * Returns:
 *  true if error
 */
private bool checkLabel(SwitchStatement ss)
{
    /*
     * Checks the scope of a label for existing variable declaration.
     * Params:
     *   vd = last variable declared before this case/default label
     * Returns: `true` if the variables declared in this label would be skipped.
     */
    bool checkVar(VarDeclaration vd)
    {
        for (auto v = vd; v && v != ss.lastVar; v = v.lastVar)
        {
            if (v.isDataseg() || (v.storage_class & (STC.manifest | STC.temp) && vd.ident != Id.withSym) || v._init.isVoidInitializer())
                continue;
            if (vd.ident == Id.withSym)
                error(ss.loc, "`switch` skips declaration of `with` temporary");
            else
                error(ss.loc, "`switch` skips declaration of variable `%s`", v.toPrettyChars());
            errorSupplemental(v.loc, "declared here");
            return true;
        }
        return false;
    }

    enum error = true;

    if (ss.sdefault && checkVar(ss.sdefault.lastVar))
        return !error; // return error once fully deprecated

    foreach (scase; *ss.cases)
    {
        if (scase && checkVar(scase.lastVar))
            return !error; // return error once fully deprecated
    }
    return !error;
}


/**************
 * Check for skipped variable declarations.
 * Params:
 *      gs = statement to check
 * Returns: true for error
 */
bool checkLabel(GotoStatement gs)
{
    if (!gs.label.statement)
        return true;        // error should have been issued for this already

    if (gs.label.statement.os != gs.os)
    {
        if (gs.os && gs.os.tok == TOK.onScopeFailure && !gs.label.statement.os)
        {
            // Jump out from scope(failure) block is allowed.
        }
        else
        {
            if (gs.label.statement.os)
                error(gs.loc, "cannot `goto` in to `%s` block", Token.toChars(gs.label.statement.os.tok));
            else
                error(gs.loc, "cannot `goto` out of `%s` block", Token.toChars(gs.os.tok));
            return true;
        }
    }

    if (gs.label.statement.tf != gs.tf)
    {
        error(gs.loc, "cannot `goto` in or out of `finally` block");
        return true;
    }

    if (gs.label.statement.inCtfeBlock && !gs.inCtfeBlock)
    {
        error(gs.loc, "cannot `goto` into `if (__ctfe)` block");
        return true;
    }

    Statement stbnext;
    for (auto stb = gs.tryBody; stb != gs.label.statement.tryBody; stb = stbnext)
    {
        if (!stb)
        {
            error(gs.loc, "cannot `goto` into `try` block");
            return true;
        }
        if (auto stf = stb.isTryFinallyStatement())
            stbnext = stf.tryBody;
        else if (auto stc = stb.isTryCatchStatement())
            stbnext = stc.tryBody;
        else
            assert(0);
    }

    VarDeclaration vd = gs.label.statement.lastVar;
    if (!vd || vd.isDataseg() || (vd.storage_class & STC.manifest))
        return false;

    VarDeclaration last = gs.lastVar;
    while (last && last != vd)
        last = last.lastVar;
    if (last == vd)
    {
        // All good, the label's scope has no variables
    }
    else if (vd.storage_class & STC.exptemp)
    {
        // Lifetime ends at end of expression, so no issue with skipping the statement
    }
    else
    {
        if (vd.ident == Id.withSym)
            error(gs.loc, "`goto` skips declaration of `with` temporary");
        else
            error(gs.loc, "`goto` skips declaration of variable `%s`", vd.toPrettyChars());
        errorSupplemental(vd.loc, "declared here");
        return true;
    }
    return false;
}
