/**
 * Does semantic analysis for statements.
 *
 * Specification: $(LINK2 https://dlang.org/spec/statement.html, Statements)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/statementsem.d, _statementsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_statementsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/statementsem.d
 */

module dmd.statementsem;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arrayop;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.ast_node;
import dmd.attrib;
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
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.escape;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.gluelayer;
import dmd.id;
import dmd.identifier;
import dmd.importc;
import dmd.init;
import dmd.intrange;
import dmd.location;
import dmd.mtype;
import dmd.mustuse;
import dmd.nogc;
import dmd.opover;
import dmd.parse;
import dmd.printast;
import dmd.common.outbuffer;
import dmd.root.string;
import dmd.semantic2;
import dmd.sideeffect;
import dmd.statement;
import dmd.staticassert;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;
import dmd.compiler;

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
    uint flags = (sc.flags & SCOPE.contract);
    const id = ident.toString();
    if (flags && flags != SCOPE.invariant_ &&
        !(id.length >= 2 && id[0] == '_' && id[1] == '_'))  // does not start with "__"
    {
        OutBuffer buf;
        buf.writestring(flags == SCOPE.require ? "__in_" : "__out_");
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
private LabelStatement checkLabeledLoop(Scope* sc, Statement statement)
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
    if (sc.flags & SCOPE.Cfile)
        return e;
    auto ec = lastComma(e);
    if (ec.op == EXP.assign)
    {
        ec.error("assignment cannot be used as a condition, perhaps `==` was meant?");
        return ErrorExp.get();
    }
    return e;
}

// Performs semantic analysis in Statement AST nodes
extern(C++) Statement statementSemantic(Statement s, Scope* sc)
{
    version (CallbackAPI)
        Compiler.onStatementSemanticStart(s, sc);

    scope v = new StatementSemanticVisitor(sc);
    s.accept(v);

    version (CallbackAPI)
        Compiler.onStatementSemanticDone(s, sc);

    return v.result;
}

package (dmd) extern (C++) final class StatementSemanticVisitor : Visitor
{
    alias visit = Visitor.visit;

    Statement result;
    Scope* sc;

    this(Scope* sc) scope
    {
        this.sc = sc;
    }

    private void setError()
    {
        result = new ErrorStatement();
    }

    override void visit(Statement s)
    {
        result = s;
    }

    override void visit(ErrorStatement s)
    {
        result = s;
    }

    override void visit(PeelStatement s)
    {
        /* "peel" off this wrapper, and don't run semantic()
         * on the result.
         */
        result = s.s;
    }

    override void visit(ExpStatement s)
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
        if (!(sc.flags & SCOPE.Cfile) && discardValue(s.exp))
            s.exp = ErrorExp.get();

        s.exp = s.exp.optimize(WANTvalue);
        s.exp = checkGC(sc, s.exp);
        if (s.exp.op == EXP.error)
            return setError();
        result = s;
    }

    override void visit(CompileStatement cs)
    {
        /* https://dlang.org/spec/statement.html#mixin-statement
         */

        //printf("CompileStatement::semantic() %s\n", exp.toChars());
        Statements* a = cs.flatten(sc);
        if (!a)
            return;
        Statement s = new CompoundStatement(cs.loc, a);
        result = s.statementSemantic(sc);
    }

    override void visit(CompoundStatement cs)
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

            Statements* flt = s.flatten(sc);
            if (flt)
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
                    if (sexception.blockExit(sc.func, false) & BE.fallthru)
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
        void flatten(Statements* statements)
        {
            for (size_t i = 0; i < statements.length;)
            {
                Statement s = (*statements)[i];
                if (s)
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
        flatten(cs.statements);

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

    override void visit(UnrolledLoopStatement uls)
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

    override void visit(ScopeStatement ss)
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

    override void visit(ForwardingStatement ss)
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

    override void visit(WhileStatement ws)
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

    override void visit(DoStatement ds)
    {
        /* https://dlang.org/spec/statement.html#do-statement
         */
        const inLoopSave = sc.inLoop;
        sc.inLoop = true;
        if (ds._body)
            ds._body = ds._body.semanticScope(sc, ds, ds, null);
        sc.inLoop = inLoopSave;

        if (ds.condition.op == EXP.dotIdentifier)
            (cast(DotIdExp)ds.condition).noderef = true;

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

    override void visit(ForStatement fs)
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
            if (fs.condition.op == EXP.dotIdentifier)
                (cast(DotIdExp)fs.condition).noderef = true;

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
                fs.increment.deprecation("`%s` has no effect", fs.increment.toChars());
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

    override void visit(ForeachStatement fs)
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
                    fs.error("cannot infer type for `foreach` variable `%s`, perhaps set it explicitly", p.ident.toChars());
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
        if (fs.aggr.type && fs.aggr.type.toBasetype().ty == Tstruct &&
            (cast(TypeStruct)(fs.aggr.type.toBasetype())).sym.dtor &&
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

            fs.error("invalid `%s` aggregate `%s` of type `%s`",
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

        /* Check for inference errors
         */
        if (!inferApplyArgTypes(fs, sc, sapply))
        {
            /**
             Try and extract the parameter count of the opApply callback function, e.g.:
             int opApply(int delegate(int, float)) => 2 args
             */
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
                            fparam.type.nextOf().ty == Tfunction)
                        {
                            TypeFunction tf = cast(TypeFunction)fparam.type.nextOf();
                            foreachParamCount = tf.parameterList.length;
                            foundMismatch = true;
                        }
                    }
                }
            }

            //printf("dim = %d, parameters.length = %d\n", dim, parameters.length);
            if (foundMismatch && dim != foreachParamCount)
            {
                const(char)* plural = foreachParamCount > 1 ? "s" : "";
                fs.error("cannot infer argument types, expected %llu argument%s, not %llu",
                    cast(ulong) foreachParamCount, plural, cast(ulong) dim);
            }
            else
                fs.error("cannot uniquely infer `foreach` argument types");

            return setError();
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
                fs.error("cannot declare `enum` loop variables for non-unrolled foreach");
            }
            if (p.storageClass & STC.alias_)
            {
                fs.error("cannot declare `alias` loop variables for non-unrolled foreach");
            }
        }

        void retError()
        {
            sc2.pop();
            result = new ErrorStatement();
        }

        void rangeError()
        {
            fs.error("cannot infer argument types");
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
                    fs.error("only one or two arguments for array `foreach`");
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
                    if (!tindex.isintegral())
                    {
                        fs.error("foreach: key cannot be of non-integral type `%s`", tindex.toChars());
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
                        fs.deprecation("foreach: loop index implicitly converted from `size_t` to `%s`",
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
                            fs.error("`foreach`: value of UTF conversion cannot be `ref`");
                            return retError();
                        }
                        if (dim == 2)
                        {
                            p = (*fs.parameters)[0];
                            if (p.storageClass & STC.ref_)
                            {
                                fs.error("`foreach`: key cannot be `ref`");
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
                            fs.error("key type mismatch, `%s` to `ref %s`",
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
                            fs.error("index type `%s` cannot cover index range 0..%llu",
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
                            fs.error("argument type mismatch, `%s` to `ref %s`",
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
                fs.warning("cannot use `foreach_reverse` with an associative array");
            if (checkForArgTypes(fs))
                return retError();

            if (dim < 1 || dim > 2)
            {
                fs.error("only one or two arguments for associative array `foreach`");
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
                    if (!fd.functionSemantic())
                        return rangeError();
                    tfront = fd.type;
                }
                else if (auto td = sfront.isTemplateDeclaration())
                {
                    Expressions a;
                    if (auto f = resolveFuncCall(loc, sc, td, null, tab, ArgumentList(&a), FuncResolveFlag.quiet))
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
                    if (!ftt.isref)
                    {
                        // .front() does not return a ref. We ignore ref on foreach arg.
                        // see https://issues.dlang.org/show_bug.cgi?id=11934
                        if (tfront.needsDestruction()) ignoreRef = true;
                    }
                }
                if (tfront.ty == Tvoid)
                {
                    fs.error("`%s.front` is `void` and has no value", oaggr.toChars());
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
                        fs.error("cannot infer argument types, expected %llu argument%s, not %llu",
                            cast(ulong) exps.length, plural, cast(ulong) dim);
                        return retError();
                    }

                    foreach (i; 0 .. dim)
                    {
                        auto p = (*fs.parameters)[i];
                        auto exp = (*exps)[i];
                        version (none)
                        {
                            printf("[%d] p = %s %s, exp = %s %s\n", i,
                                p.type ? p.type.toChars() : "?", p.ident.toChars(),
                                exp.type.toChars(), exp.toChars());
                        }
                        if (!p.type)
                            p.type = exp.type;

                        auto sc = p.storageClass;
                        if (ignoreRef) sc &= ~STC.ref_;
                        p.type = p.type.addStorageClass(sc).typeSemantic(loc, sc2);
                        if (!exp.implicitConvTo(p.type))
                            return rangeError();

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
                    printf("init: %s\n", _init.toChars());
                    printf("condition: %s\n", condition.toChars());
                    printf("increment: %s\n", increment.toChars());
                    printf("body: %s\n", forbody.toChars());
                }
                return retStmt(s);
            }
        case Tdelegate:
            if (fs.op == TOK.foreach_reverse_)
                fs.deprecation("cannot use `foreach_reverse` with a delegate");
            return retStmt(apply());
        case Terror:
            return retError();
        default:
            fs.error("`foreach`: `%s` is not an aggregate type", fs.aggr.type.toChars());
            return retError();
        }
    }

    private static extern(D) Expression applyOpApply(ForeachStatement fs, Expression flde,
                Type tab, Scope* sc2, Dsymbol sapply)
    {
        version (none)
        {
            if (global.params.useDIP1000 == FeatureState.enabled)
            {
                message(loc, "To enforce `@safe`, the compiler allocates a closure unless `opApply()` uses `scope`");
            }
            (cast(FuncExp)flde).fd.tookAddressOf = 1;
        }
        else
        {
            if (global.params.useDIP1000 == FeatureState.enabled)
                ++(cast(FuncExp)flde).fd.tookAddressOf;  // allocate a closure unless the opApply() uses 'scope'
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
        if (ec.op == EXP.error)
            return null;
        if (ec.type != Type.tint32)
        {
            fs.error("`opApply()` function for `%s` must return an `int`", tab.toChars());
            return null;
        }
        return ec;
    }

    private static extern(D) Expression applyDelegate(ForeachStatement fs, Expression flde,
                                                      Type tab, Scope* sc2)
    {
        Expression ec;
        /* Call:
         *      aggr(flde)
         */
        if (fs.aggr.op == EXP.delegate_ && (cast(DelegateExp)fs.aggr).func.isNested() &&
            !(cast(DelegateExp)fs.aggr).func.needThis())
        {
            // https://issues.dlang.org/show_bug.cgi?id=3560
            fs.aggr = (cast(DelegateExp)fs.aggr).e1;
        }
        ec = new CallExp(fs.loc, fs.aggr, flde);
        ec = ec.expressionSemantic(sc2);
        if (ec.op == EXP.error)
            return null;
        if (ec.type != Type.tint32)
        {
            fs.error("`opApply()` function for `%s` must return an `int`", tab.toChars());
            return null;
        }
        return ec;
    }

    private static extern(D) Expression applyArray(ForeachStatement fs, Expression flde,
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
        params.push(new Parameter(STC.in_, tn.arrayOf(), null, null, null));
        auto dgparams = new Parameters();
        dgparams.push(new Parameter(0, Type.tvoidptr, null, null, null));
        if (dim == 2)
            dgparams.push(new Parameter(0, Type.tvoidptr, null, null, null));
        dgty = new TypeDelegate(new TypeFunction(ParameterList(dgparams), Type.tint32, LINK.d));
        params.push(new Parameter(0, dgty, null, null, null));
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

    private static extern(D) Expression applyAssocArray(ForeachStatement fs, Expression flde, Type tab)
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
                fs.error("`foreach`: index must be type `%s`, not `%s`",
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
            fs.error("`foreach`: value must be type `%s`, not `%s`",
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
            params.push(new Parameter(0, Type.tvoid.pointerTo(), null, null, null));
            params.push(new Parameter(STC.const_, Type.tsize_t, null, null, null));
            auto dgparams = new Parameters();
            dgparams.push(new Parameter(0, Type.tvoidptr, null, null, null));
            if (dim == 2)
                dgparams.push(new Parameter(0, Type.tvoidptr, null, null, null));
            fldeTy[i] = new TypeDelegate(new TypeFunction(ParameterList(dgparams), Type.tint32, LINK.d));
            params.push(new Parameter(0, fldeTy[i], null, null, null));
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

    private static extern(D) Statement loopReturn(Expression e, Statements* cases, const ref Loc loc)
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
        return new SwitchStatement(loc, e, s, false);
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
    static FuncExp foreachBodyToFunction(Scope* sc, ForeachStatement fs, TypeFunction tfld)
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
                Parameter prm = tfld.parameterList[i];
                //printf("\tprm = %s%s\n", (prm.storageClass&STC.ref_?"ref ":"").ptr, prm.ident.toChars());
                stc = (prm.storageClass & STC.ref_) | (p.storageClass & STC.scope_);
                if ((p.storageClass & STC.ref_) != (prm.storageClass & STC.ref_))
                {
                    if (!(prm.storageClass & STC.ref_))
                    {
                        fs.error("`foreach`: cannot make `%s` `ref`", p.ident.toChars());
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
            params.push(new Parameter(stc, p.type, id, null, null));
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
        return cast(FuncExp)flde;
    }

    override void visit(ForeachRangeStatement fs)
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
            fs.error("invalid range lower bound `%s`", fs.lwr.toChars());
            return setError();
        }

        fs.upr = fs.upr.expressionSemantic(sc);
        fs.upr = resolveProperties(sc, fs.upr);
        fs.upr = fs.upr.optimize(WANTvalue);
        if (!fs.upr.type)
        {
            fs.error("invalid range upper bound `%s`", fs.upr.toChars());
            return setError();
        }

        if (fs.prm.type)
        {
            fs.prm.type = fs.prm.type.typeSemantic(loc, sc);
            fs.prm.type = fs.prm.type.addStorageClass(fs.prm.storageClass);
            fs.lwr = fs.lwr.implicitCastTo(sc, fs.prm.type);

            if (fs.upr.implicitConvTo(fs.prm.type) || (fs.prm.storageClass & STC.ref_))
            {
                fs.upr = fs.upr.implicitCastTo(sc, fs.prm.type);
            }
            else
            {
                // See if upr-1 fits in prm.type
                Expression limit = new MinExp(loc, fs.upr, IntegerExp.literal!1);
                limit = limit.expressionSemantic(sc);
                limit = limit.optimize(WANTvalue);
                if (!limit.implicitConvTo(fs.prm.type))
                {
                    fs.upr = fs.upr.implicitCastTo(sc, fs.prm.type);
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
                fs.prm.type = fs.lwr.type;
            }
            else if (fs.lwr.type == fs.upr.type)
            {
                /* Same logic as CondExp ?lwr:upr
                 */
                fs.prm.type = fs.lwr.type;
            }
            else
            {
                scope AddExp ea = new AddExp(loc, fs.lwr, fs.upr);
                if (typeCombine(ea, sc))
                    return setError();
                fs.prm.type = ea.type;
                fs.lwr = ea.e1;
                fs.upr = ea.e2;
            }
            fs.prm.type = fs.prm.type.addStorageClass(fs.prm.storageClass);
        }
        if (fs.prm.type.ty == Terror || fs.lwr.op == EXP.error || fs.upr.op == EXP.error)
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
            if (fs.prm.type.isscalar())
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
            if (fs.prm.type.isscalar())
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
        if ((fs.prm.storageClass & STC.ref_) && fs.prm.type.equals(fs.key.type))
        {
            fs.key.range = null;
            auto v = new AliasDeclaration(loc, fs.prm.ident, fs.key);
            fs._body = new CompoundStatement(loc, new ExpStatement(loc, v), fs._body);
        }
        else
        {
            ie = new ExpInitializer(loc, new CastExp(loc, new VarExp(loc, fs.key), fs.prm.type));
            auto v = new VarDeclaration(loc, fs.prm.type, fs.prm.ident, ie);
            v.storage_class |= STC.temp | STC.foreach_ | (fs.prm.storageClass & STC.ref_);
            fs._body = new CompoundStatement(loc, new ExpStatement(loc, v), fs._body);
            if (fs.key.range && !fs.prm.type.isMutable())
            {
                /* Limit the range of the key to the specified range
                 */
                v.range = new IntRange(fs.key.range.imin, fs.key.range.imax - SignExtendedNumber(1));
            }
        }
        if (fs.prm.storageClass & STC.ref_)
        {
            if (fs.key.type.constConv(fs.prm.type) == MATCH.nomatch)
            {
                fs.error("argument type mismatch, `%s` to `ref %s`", fs.key.type.toChars(), fs.prm.type.toChars());
                return setError();
            }
        }

        auto s = new ForStatement(loc, forinit, cond, increment, fs._body, fs.endloc);
        if (LabelStatement ls = checkLabeledLoop(sc, fs))
            ls.gotoTarget = s;
        result = s.statementSemantic(sc);
    }

    override void visit(IfStatement ifs)
    {
        /* https://dlang.org/spec/statement.html#IfStatement
         */

        // check in syntax level
        ifs.condition = checkAssignmentAsCondition(ifs.condition, sc);

        auto sym = new ScopeDsymbol();
        sym.parent = sc.scopesym;
        sym.endlinnum = ifs.endloc.linnum;
        Scope* scd = sc.push(sym);
        if (ifs.prm)
        {
            /* Declare prm, which we will set to be the
             * result of condition.
             */
            auto ei = new ExpInitializer(ifs.loc, ifs.condition);
            ifs.match = new VarDeclaration(ifs.loc, ifs.prm.type, ifs.prm.ident, ei);
            ifs.match.parent = scd.func;
            ifs.match.storage_class |= ifs.prm.storageClass;
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
            if (ifs.condition.op == EXP.dotIdentifier)
                (cast(DotIdExp)ifs.condition).noderef = true;

            ifs.condition = ifs.condition.expressionSemantic(scd);
            ifs.condition = resolveProperties(scd, ifs.condition);
            ifs.condition = ifs.condition.addDtorHook(scd);
        }
        if (checkNonAssignmentArrayOp(ifs.condition))
            ifs.condition = ErrorExp.get();

        // Convert to boolean after declaring prm so this works:
        //  if (S prm = S()) {}
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

        /* Detect `if (__ctfe)`
         */
        if (ifs.isIfCtfeBlock())
        {
            Scope* scd2 = scd.push();
            scd2.flags |= SCOPE.ctfeBlock;
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

    override void visit(ConditionalStatement cs)
    {
        //printf("ConditionalStatement::semantic()\n");

        // If we can short-circuit evaluate the if statement, don't do the
        // semantic analysis of the skipped code.
        // This feature allows a limited form of conditional compilation.
        if (cs.condition.include(sc))
        {
            DebugCondition dc = cs.condition.isDebugCondition();
            if (dc)
            {
                sc = sc.push();
                sc.flags |= SCOPE.debug_;
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

    override void visit(PragmaStatement ps)
    {
        /* https://dlang.org/spec/statement.html#pragma-statement
         */
        // Should be merged with PragmaDeclaration

        //printf("PragmaStatement::semantic() %s\n", ps.toChars());
        //printf("body = %p\n", ps._body);
        if (ps.ident == Id.msg)
        {
            if (!pragmaMsgSemantic(ps.loc, sc, ps.args))
                return setError();
        }
        else if (ps.ident == Id.lib)
        {
            version (all)
            {
                /* Should this be allowed?
                 */
                ps.error("`pragma(lib)` not allowed as statement");
                return setError();
            }
            else
            {
                if (!ps.args || ps.args.length != 1)
                {
                    ps.error("`string` expected for library name");
                    return setError();
                }
                else
                {
                    auto se = semanticString(sc, (*ps.args)[0], "library name");
                    if (!se)
                        return setError();

                    if (global.params.verbose)
                    {
                        message("library   %.*s", cast(int)se.len, se.string);
                    }
                }
            }
        }
        else if (ps.ident == Id.linkerDirective)
        {
            /* Should this be allowed?
             */
            ps.error("`pragma(linkerDirective)` not allowed as statement");
            return setError();
        }
        else if (ps.ident == Id.startaddress)
        {
            if (!pragmaStartAddressSemantic(ps.loc, sc, ps.args))
                return setError();
        }
        else if (ps.ident == Id.Pinline)
        {
            if (auto fd = sc.func)
            {
                fd.inlining = evalPragmaInline(ps.loc, sc, ps.args);
            }
            else
            {
                ps.error("`pragma(inline)` is not inside a function");
                return setError();
            }
        }
        else if (!global.params.ignoreUnsupportedPragmas)
        {
            ps.error("unrecognized `pragma(%s)`", ps.ident.toChars());
            return setError();
        }

        if (ps._body)
        {
            if (ps.ident == Id.msg || ps.ident == Id.startaddress)
            {
                ps.error("`pragma(%s)` is missing a terminating `;`", ps.ident.toChars());
                return setError();
            }
            ps._body = ps._body.statementSemantic(sc);
        }
        result = ps._body;
    }

    override void visit(StaticAssertStatement s)
    {
        s.sa.semantic2(sc);
        if (s.sa.errors)
            return setError();
    }

    override void visit(SwitchStatement ss)
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

        bool conditionError = false;
        ss.condition = ss.condition.expressionSemantic(sc);
        ss.condition = resolveProperties(sc, ss.condition);

        Type att = null;
        TypeEnum te = null;
        while (!ss.condition.isErrorExp())
        {
            // preserve enum type for final switches
            if (ss.condition.type.ty == Tenum)
                te = cast(TypeEnum)ss.condition.type;
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
            if (!ss.condition.isErrorExp() && ss.condition.type.isintegral())
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
                ss.error("`%s` must be of integral or string type, it is a `%s`",
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
                gcs.error("no `case` statement following `goto case;`");
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
            gcs.error("`case %s` not found", gcs.exp.toChars());
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
                const maxShown = !global.params.verbose ?
                                    (global.params.errorSupplementLimit ? global.params.errorSupplementLimit : int.max)
                                    : int.max;
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
                            ss.error("missing cases for `enum` members in `final switch`:");

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

        if (!sc.sw.sdefault &&
            (!ss.isFinal || needswitcherror || global.params.useAssert == CHECKENABLE.on || sc.func.isSafe))
        {
            ss.hasNoDefault = 1;

            if (!ss.isFinal && (!ss._body || !ss._body.isErrorStatement()) && !(sc.flags & SCOPE.Cfile))
                ss.error("`switch` statement without a `default`; use `final switch` or add `default: assert(0);` or add `default: break;`");

            // Generate runtime error if the default is hit
            auto a = new Statements();
            CompoundStatement cs;
            Statement s;

            if (sc.flags & SCOPE.Cfile)
            {
                s = new BreakStatement(ss.loc, null);   // default for C is `default: break;`
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
            if (ss._body.blockExit(sc.func, false) & BE.fallthru)
                a.push(new BreakStatement(Loc.initial, null));
            a.push(sc.sw.sdefault);
            cs = new CompoundStatement(ss.loc, a);
            ss._body = cs;
        }

        if (!(sc.flags & SCOPE.Cfile) && ss.checkLabel())
        {
            sc.pop();
            return setError();
        }


        if (!ss.condition.type.isString())
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

    override void visit(CaseStatement cs)
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
            while (e.op == EXP.cast_)
                e = (cast(CastExp)e).e1;

            /* This is where variables are allowed as case expressions.
            */
            if (e.op == EXP.variable)
            {
                VarExp ve = cast(VarExp)e;
                VarDeclaration v = ve.var.isVarDeclaration();
                Type t = cs.exp.type.toBasetype();
                if (v && (t.isintegral() || t.ty == Tclass))
                {
                    /* Flag that we need to do special code generation
                    * for this, i.e. generate a sequence of if-then-else
                    */
                    sw.hasVars = 1;

                    /* TODO check if v can be uninitialized at that point.
                    */
                    if (!v.isConst() && !v.isImmutable())
                    {
                        cs.error("`case` variables have to be `const` or `immutable`");
                    }

                    if (sw.isFinal)
                    {
                        cs.error("`case` variables not allowed in `final switch` statements");
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

                        if (!scx.search(cs.exp.loc, v.ident, null))
                        {
                            cs.error("`case` variable `%s` declared at %s cannot be declared in `switch` body",
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
                cs.error("`case` expression must be a compile-time `string` or an integral constant, not `%s`", cs.exp.toChars());
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
                    cs.error("duplicate `case %s` in `switch` statement", initialExp.toChars());
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
                cs.error("`switch` and `case` are in different `finally` blocks");
                errors = true;
            }
            if (sc.sw.tryBody != sc.tryBody)
            {
                cs.error("case cannot be in different `try` block level from `switch`");
                errors = true;
            }
        }
        else
        {
            cs.error("`case` not in `switch` statement");
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

    override void visit(CaseRangeStatement crs)
    {
        SwitchStatement sw = sc.sw;
        if (sw is null)
        {
            crs.error("case range not in `switch` statement");
            return setError();
        }

        //printf("CaseRangeStatement::semantic() %s\n", toChars());
        bool errors = false;
        if (sw.isFinal)
        {
            crs.error("case ranges not allowed in `final switch`");
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
        if ((crs.first.type.isunsigned() && fval > lval) || (!crs.first.type.isunsigned() && cast(sinteger_t)fval > cast(sinteger_t)lval))
        {
            crs.error("first `case %s` is greater than last `case %s`", crs.first.toChars(), crs.last.toChars());
            errors = true;
            lval = fval;
        }

        if (lval - fval > 256)
        {
            crs.error("had %llu cases which is more than 257 cases in case range", 1 + lval - fval);
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

    override void visit(DefaultStatement ds)
    {
        //printf("DefaultStatement::semantic()\n");
        bool errors = false;
        if (sc.sw)
        {
            if (sc.sw.sdefault)
            {
                ds.error("`switch` statement already has a default");
                errors = true;
            }
            sc.sw.sdefault = ds;

            if (sc.sw.tf != sc.tf)
            {
                ds.error("`switch` and `default` are in different `finally` blocks");
                errors = true;
            }
            if (sc.sw.tryBody != sc.tryBody)
            {
                ds.error("default cannot be in different `try` block level from `switch`");
                errors = true;
            }
            if (sc.sw.isFinal)
            {
                ds.error("`default` statement not allowed in `final switch` statement");
                errors = true;
            }
        }
        else
        {
            ds.error("`default` not in `switch` statement");
            errors = true;
        }

        sc.ctorflow.orCSX(CSX.label);
        ds.statement = ds.statement.statementSemantic(sc);
        if (errors || ds.statement.isErrorStatement())
            return setError();

        ds.lastVar = sc.lastVar;
        result = ds;
    }

    override void visit(GotoDefaultStatement gds)
    {
        /* https://dlang.org/spec/statement.html#goto-statement
         */

        gds.sw = sc.sw;
        if (!gds.sw)
        {
            gds.error("`goto default` not in `switch` statement");
            return setError();
        }
        if (gds.sw.isFinal)
        {
            gds.error("`goto default` not allowed in `final switch` statement");
            return setError();
        }
        result = gds;
    }

    override void visit(GotoCaseStatement gcs)
    {
        /* https://dlang.org/spec/statement.html#goto-statement
         */

        if (!sc.sw)
        {
            gcs.error("`goto case` not in `switch` statement");
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

    override void visit(ReturnStatement rs)
    {
        /* https://dlang.org/spec/statement.html#return-statement
         */

        //printf("ReturnStatement.dsymbolSemantic() %p, %s\n", rs, rs.toChars());

        FuncDeclaration fd = sc.parent.isFuncDeclaration();
        if (fd.fes)
            fd = fd.fes.func; // fd is now function enclosing foreach

            TypeFunction tf = cast(TypeFunction)fd.type;
        assert(tf.ty == Tfunction);

        if (rs.exp && rs.exp.op == EXP.variable && (cast(VarExp)rs.exp).var == fd.vresult)
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

        bool inferRef = (tf.isref && (fd.storage_class & STC.auto_));
        Expression e0 = null;

        bool errors = false;
        if (sc.flags & SCOPE.contract)
        {
            rs.error("`return` statements cannot be in contracts");
            errors = true;
        }
        if (sc.os)
        {
            // @@@DEPRECATED_2.112@@@
            // Deprecated in 2.100, transform into an error in 2.112
            if (sc.os.tok == TOK.onScopeFailure)
            {
                rs.deprecation("`return` statements cannot be in `scope(failure)` bodies.");
                deprecationSupplemental(rs.loc, "Use try-catch blocks for this purpose");
            }
            else
            {
                rs.error("`return` statements cannot be in `%s` bodies", Token.toChars(sc.os.tok));
                errors = true;
            }
        }
        if (sc.tf)
        {
            rs.error("`return` statements cannot be in `finally` bodies");
            errors = true;
        }

        if (fd.isCtorDeclaration())
        {
            if (rs.exp)
            {
                rs.error("cannot return expression from constructor");
                errors = true;
            }

            // Constructors implicitly do:
            //      return this;
            rs.exp = new ThisExp(Loc.initial);
            rs.exp.type = tret;
        }
        else if (rs.exp)
        {
            fd.hasReturnExp |= (fd.hasReturnExp & 1 ? 16 : 1);

            FuncLiteralDeclaration fld = fd.isFuncLiteralDeclaration();
            if (tret)
                rs.exp = inferType(rs.exp, tret);
            else if (fld && fld.treq)
                rs.exp = inferType(rs.exp, fld.treq.nextOf().nextOf());

            rs.exp = rs.exp.expressionSemantic(sc);
            rs.exp = rs.exp.arrayFuncConv(sc);
            // If we're returning by ref, allow the expression to be `shared`
            const returnSharedRef = (tf.isref && (fd.inferRetType || tret.isShared()));
            rs.exp.checkSharedAccess(sc, returnSharedRef);

            // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
            if (rs.exp.op == EXP.type)
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
            if (rs.exp.op == EXP.call)
                rs.exp = valueNoDtor(rs.exp);

            /* Void-return function can have void / noreturn typed expression
             * on return statement.
             */
            auto texp = rs.exp.type;
            const convToVoid = texp.ty == Tvoid || texp.ty == Tnoreturn;

            if (tbret && tbret.ty == Tvoid || convToVoid)
            {
                if (!convToVoid)
                {
                    rs.error("cannot return non-void from `void` function");
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
                    rs.exp.deprecation("`%s` has no effect", rs.exp.toChars());

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
                else if (tret.ty != Terror && !rs.exp.type.equals(tret))
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
                        rs.error("expected return type of `%s`, not `%s`:",
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
                    tf.isref = false;    // return by value
                    tf.isreturn = false; // ignore 'return' attribute, whether explicit or inferred
                    fd.storage_class &= ~STC.return_;

                    // If we previously assumed the function could be ref when
                    // checking for `shared`, make sure we were right
                    if (global.params.noSharedAccess == FeatureState.enabled && rs.exp.type.isShared())
                    {
                        fd.error("function returns `shared` but cannot be inferred `ref`");
                        supplemental();
                    }
                }

                if (rs.exp.isLvalue())
                {
                    /* May return by ref
                     */
                    if (checkReturnEscapeRef(sc, rs.exp, true))
                        turnOffRef(() { checkReturnEscapeRef(sc, rs.exp, false); });
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
                    if (tf.next.ty != Terror)
                    {
                        rs.error("mismatched function return type inference of `void` and `%s`", tf.next.toChars());
                    }
                    errors = true;
                    tf.next = Type.terror;
                }

                tret = tf.next;
                tbret = tret.toBasetype();
            }

            if (inferRef) // deduce 'auto ref'
                tf.isref = false;

            if (tbret.ty != Tvoid && !resType.isTypeNoreturn()) // if non-void return
            {
                if (tbret.ty != Terror)
                {
                    if (e0)
                        rs.error("expected return type of `%s`, not `%s`", tret.toChars(), resType.toChars());
                    else
                        rs.error("`return` expression expected");
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
            rs.error("`return` without calling constructor");
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
                    rs.error("an earlier `return` statement skips field `%s` initialization", v.toChars());
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
            if (e0.op == EXP.declaration || e0.op == EXP.comma)
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

    override void visit(BreakStatement bs)
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
                        bs.error("label `%s` has no `break`", bs.ident.toChars());
                    else if (ls.tf != sc.tf)
                        bs.error("cannot break out of `finally` block");
                    else
                    {
                        ls.breaks = true;
                        result = bs;
                        return;
                    }
                    return setError();
                }
            }
            bs.error("enclosing label `%s` for `break` not found", bs.ident.toChars());
            return setError();
        }
        else if (!sc.sbreak)
        {
            if (sc.os && sc.os.tok != TOK.onScopeFailure)
            {
                bs.error("`break` is not allowed inside `%s` bodies", Token.toChars(sc.os.tok));
            }
            else if (sc.fes)
            {
                // Replace break; with return 1;
                result = new ReturnStatement(Loc.initial, IntegerExp.literal!1);
                return;
            }
            else
                bs.error("`break` is not inside a loop or `switch`");
            return setError();
        }
        else if (sc.sbreak.isForwardingStatement())
        {
            bs.error("must use labeled `break` within `static foreach`");
        }
        result = bs;
    }

    override void visit(ContinueStatement cs)
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
                        cs.error("label `%s` has no `continue`", cs.ident.toChars());
                    else if (ls.tf != sc.tf)
                        cs.error("cannot continue out of `finally` block");
                    else
                    {
                        result = cs;
                        return;
                    }
                    return setError();
                }
            }
            cs.error("enclosing label `%s` for `continue` not found", cs.ident.toChars());
            return setError();
        }
        else if (!sc.scontinue)
        {
            if (sc.os && sc.os.tok != TOK.onScopeFailure)
            {
                cs.error("`continue` is not allowed inside `%s` bodies", Token.toChars(sc.os.tok));
            }
            else if (sc.fes)
            {
                // Replace continue; with return 0;
                result = new ReturnStatement(Loc.initial, IntegerExp.literal!0);
                return;
            }
            else
                cs.error("`continue` is not inside a loop");
            return setError();
        }
        else if (sc.scontinue.isForwardingStatement())
        {
            cs.error("must use labeled `continue` within `static foreach`");
        }
        result = cs;
    }

    override void visit(SynchronizedStatement ss)
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
                ss.error("can only `synchronize` on class objects, not `%s`", ss.exp.type.toChars());
                return setError();
            }
            else if (cd.isInterfaceDeclaration())
            {
                /* Cast the interface to an object, as the object has the monitor,
                 * not the interface.
                 */
                if (!ClassDeclaration.object)
                {
                    ss.error("missing or corrupt object.d");
                    fatal();
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
                args.push(new Parameter(0, ClassDeclaration.object.type, null, null, null));

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
            enterArgs.push(new Parameter(0, t.pointerTo(), null, null, null));

            FuncDeclaration fdenter = FuncDeclaration.genCfunc(enterArgs, Type.tvoid, Id.criticalenter, STC.nothrow_);
            Expression e = new AddrExp(ss.loc, tmpExp);
            e = e.expressionSemantic(sc);
            e = new CallExp(ss.loc, fdenter, e);
            e.type = Type.tvoid; // do not run semantic on e
            cs.push(new ExpStatement(ss.loc, e));

            auto exitArgs = new Parameters();
            exitArgs.push(new Parameter(0, t, null, null, null));

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

    override void visit(WithStatement ws)
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
        else if (ws.exp.op == EXP.type)
        {
            Dsymbol s = (cast(TypeExp)ws.exp).type.toDsymbol(sc);
            if (!s || !s.isScopeDsymbol())
            {
                ws.error("`with` type `%s` has no members", ws.exp.toChars());
                return setError();
            }
            sym = new WithScopeSymbol(ws);
            sym.parent = sc.scopesym;
            sym.endlinnum = ws.endloc.linnum;
        }
        else
        {
            Type t = ws.exp.type.toBasetype();

            Expression olde = ws.exp;
            if (t.ty == Tpointer)
            {
                ws.exp = new PtrExp(ws.loc, ws.exp);
                ws.exp = ws.exp.expressionSemantic(sc);
                t = ws.exp.type.toBasetype();
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
            else
            {
                ws.error("`with` expressions must be aggregate types or pointers to them, not `%s`", olde.type.toChars());
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
    override void visit(TryCatchStatement tcs)
    {
        //printf("TryCatchStatement.semantic()\n");

        if (!global.params.useExceptions)
        {
            tcs.error("cannot use try-catch statements with -betterC");
            return setError();
        }

        if (!ClassDeclaration.throwable)
        {
            tcs.error("cannot use try-catch statements because `object.Throwable` was not declared");
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
                    tcs.error("`catch` at %s hides `catch` at %s", sj, si);
                    catchErrors = true;
                }
            }
        }

        if (sc.func)
        {
            sc.func.hasCatches = true;
            if (flags == (FLAGcpp | FLAGd))
            {
                tcs.error("cannot mix catching D and C++ exceptions in the same try-catch");
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
        if (!(tcs._body.blockExit(sc.func, false) & BE.throw_) && ClassDeclaration.exception)
        {
            foreach_reverse (i; 0 .. tcs.catches.length)
            {
                Catch c = (*tcs.catches)[i];

                /* If catch exception type is derived from Exception
                 */
                if (c.type.toBasetype().implicitConvTo(ClassDeclaration.exception.type) &&
                    (!c.handler || !c.handler.comeFrom()) && !(sc.flags & SCOPE.debug_))
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

    override void visit(TryFinallyStatement tfs)
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

        auto blockexit = tfs._body.blockExit(sc.func, false);

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

    override void visit(ScopeGuardStatement oss)
    {
        /* https://dlang.org/spec/statement.html#scope-guard-statement
         */

        if (oss.tok != TOK.onScopeExit)
        {
            // https://issues.dlang.org/show_bug.cgi?id=23159
            if (!global.params.useExceptions)
            {
                oss.error("`%s` cannot be used with -betterC", Token.toChars(oss.tok));
                return setError();
            }

            // scope(success) and scope(failure) are rewritten to try-catch(-finally) statement,
            // so the generated catch block cannot be placed in finally block.
            // See also Catch::semantic.
            if (sc.os && sc.os.tok != TOK.onScopeFailure)
            {
                // If enclosing is scope(success) or scope(exit), this will be placed in finally block.
                oss.error("cannot put `%s` statement inside `%s`", Token.toChars(oss.tok), Token.toChars(sc.os.tok));
                return setError();
            }
            if (sc.tf)
            {
                oss.error("cannot put `%s` statement inside `finally` block", Token.toChars(oss.tok));
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

    override void visit(ThrowStatement ts)
    {
        /* https://dlang.org/spec/statement.html#throw-statement
         */

        //printf("ThrowStatement::semantic()\n");
        if (throwSemantic(ts.loc, ts.exp, sc))
            result = ts;
        else
            setError();

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
    extern(D) static bool throwSemantic(const ref Loc loc, ref Expression exp, Scope* sc)
    {
        if (!global.params.useExceptions)
        {
            loc.error("cannot use `throw` statements with -betterC");
            return false;
        }

        if (!ClassDeclaration.throwable)
        {
            loc.error("cannot use `throw` statements because `object.Throwable` was not declared");
            return false;
        }

        if (FuncDeclaration fd = sc.parent.isFuncDeclaration())
            fd.hasReturnExp |= 2;

        if (exp.op == EXP.new_)
        {
            NewExp ne = cast(NewExp) exp;
            ne.thrownew = true;
        }

        exp = exp.expressionSemantic(sc);
        exp = resolveProperties(sc, exp);
        exp = checkGC(sc, exp);
        if (exp.op == EXP.error)
            return false;

        checkThrowEscape(sc, exp, false);

        ClassDeclaration cd = exp.type.toBasetype().isClassHandle();
        if (!cd || ((cd != ClassDeclaration.throwable) && !ClassDeclaration.throwable.isBaseOf(cd, null)))
        {
            loc.error("can only throw class objects derived from `Throwable`, not type `%s`", exp.type.toChars());
            return false;
        }
        return true;
    }

    override void visit(DebugStatement ds)
    {
        if (ds.statement)
        {
            sc = sc.push();
            sc.flags |= SCOPE.debug_;
            ds.statement = ds.statement.statementSemantic(sc);
            sc.pop();
        }
        result = ds.statement;
    }

    override void visit(GotoStatement gs)
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
        gs.inCtfeBlock = (sc.flags & SCOPE.ctfeBlock) != 0;

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
        else if (!(sc.flags & SCOPE.Cfile) && gs.checkLabel())
            return setError();

        result = gs;
    }

    override void visit(LabelStatement ls)
    {
        //printf("LabelStatement::semantic()\n");
        FuncDeclaration fd = sc.parent.isFuncDeclaration();

        ls.ident = fixupLabelName(sc, ls.ident);
        ls.tryBody = sc.tryBody;
        ls.tf = sc.tf;
        ls.os = sc.os;
        ls.lastVar = sc.lastVar;
        ls.inCtfeBlock = (sc.flags & SCOPE.ctfeBlock) != 0;

        LabelDsymbol ls2 = fd.searchLabel(ls.ident, ls.loc);
        if (ls2.statement)
        {
            ls.error("label `%s` already defined", ls2.toChars());
            return setError();
        }
        else
            ls2.statement = ls;

        sc = sc.push();
        sc.scopesym = sc.enclosing.scopesym;

        sc.ctorflow.orCSX(CSX.label);

        sc.slabel = ls;
        if (ls.statement)
            ls.statement = ls.statement.statementSemantic(sc);
        sc.pop();

        result = ls;
    }

    override void visit(AsmStatement s)
    {
        /* https://dlang.org/spec/statement.html#asm
         */

        //printf("AsmStatement()::semantic()\n");
        result = asmSemantic(s, sc);
    }

    override void visit(CompoundAsmStatement cas)
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
        if (!(cas.stc & STC.pure_) && sc.func.setImpure())
            cas.error("`asm` statement is assumed to be impure - mark it with `pure` if it is not");
        if (!(cas.stc & STC.nogc) && sc.func.setGC())
            cas.error("`asm` statement is assumed to use the GC - mark it with `@nogc` if it does not");
        if (!(cas.stc & (STC.trusted | STC.safe)))
        {
            sc.setUnsafe(false, cas.loc, "`asm` statement is assumed to be `@system` - mark it with `@trusted` if it is not");
        }

        sc.pop();
        result = cas;
    }

    override void visit(ImportStatement imps)
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
            if (sc.setUnsafe(false, c.loc, "cannot catch C++ class objects in `@safe` code"))
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
                "can only catch class objects derived from `Exception` in `@safe` code, not `%s`", c.type))
    {
        c.errors = true;
    }
    else if (global.params.ehnogc)
    {
        stc |= STC.scope_;
    }

    // DIP1008 requires destruction of the Throwable, even if the user didn't specify an identifier
    auto ident = c.ident;
    if (!ident && global.params.ehnogc)
        ident = Identifier.generateAnonymousId("var");

    if (ident)
    {
        c.var = new VarDeclaration(c.loc, c.type, ident, null, stc);
        c.var.iscatchvar = true;
        c.var.dsymbolSemantic(sc);
        sc.insert(c.var);

        if (global.params.ehnogc && stc & STC.scope_)
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
        if (es.exp && es.exp.op == EXP.declaration)
        {
            auto de = cast(DeclarationExp)es.exp;
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
        fs.error("only one (value) or two (key,value) arguments for tuple `foreach`");
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
    TypeTuple tuple = cast(TypeTuple)tab;

    Statements* statements;
    Dsymbols* declarations;
    if (isDecl)
        declarations = new Dsymbols();
    else
        statements = new Statements();

    //printf("aggr: op = %d, %s\n", fs.aggr.op, fs.aggr.toChars());
    size_t n;
    TupleExp te = null;
    if (fs.aggr.op == EXP.tuple) // expression tuple
    {
        te = cast(TupleExp)fs.aggr;
        n = te.exps.length;
    }
    else if (fs.aggr.op == EXP.type) // type tuple
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
                fs.error("no storage class for key `%s`", p.ident.toChars());
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

            if (!p.type.isintegral())
            {
                fs.error("foreach: key cannot be of non-integral type `%s`",
                         p.type.toChars());
                return returnEarly();
            }

            const length = te ? te.exps.length : tuple.arguments.length;
            IntRange dimrange = IntRange(SignExtendedNumber(length))._cast(Type.tsize_t);
            // https://issues.dlang.org/show_bug.cgi?id=12504
            dimrange.imax = SignExtendedNumber(dimrange.imax.value-1);
            if (!IntRange.fromType(p.type).contains(dimrange))
            {
                fs.error("index type `%s` cannot cover index range 0..%llu",
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
                fs.error("no storage class for value `%s`", ident.toChars());
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
                    fs.error("`foreach` loop variable cannot be both `enum` and `alias`");
                    return false;
                }

                if (ds)
                {
                    var = new AliasDeclaration(loc, ident, ds);
                    if (storageClass & STC.ref_)
                    {
                        fs.error("symbol `%s` cannot be `ref`", ds.toChars());
                        return false;
                    }
                    if (paramtype)
                    {
                        fs.error("cannot specify element type for symbol `%s`", ds.toChars());
                        return false;
                    }
                }
                else if (e.op == EXP.type)
                {
                    var = new AliasDeclaration(loc, ident, e.type);
                    if (paramtype)
                    {
                        fs.error("cannot specify element type for type `%s`", e.type.toChars());
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
                                fs.error("constant value `%s` cannot be `ref`", ie.toChars());
                            }
                            else
                            {
                                if (!needExpansion)
                                {
                                    fs.error("constant value `%s` cannot be `ref`", ie.toChars());
                                }
                                else
                                {
                                    fs.error("constant value `%s` cannot be `ref`", ident.toChars());
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
                    fs.error("cannot specify element type for symbol `%s`", fs.toChars());
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

        case STMT.Compile:
            auto cs = statement.isCompileStatement();


            OutBuffer buf;
            if (expressionsToString(buf, sc, cs.exps))
                return errorStatements();

            const errors = global.errors;
            const len = buf.length;
            buf.writeByte(0);
            const str = buf.extractSlice()[0 .. len];
            scope p = new Parser!ASTCodegen(cs.loc, sc._module, str, false, global.errorSink);
            p.nextToken();

            auto a = new Statements();
            while (p.token.value != TOK.endOfFile)
            {
                Statement s = p.parseStatement(ParseStatementFlags.semi | ParseStatementFlags.curlyScope);
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

/***********************************************************
 * Evaluate and print a `pragma(msg, args)`
 *
 * Params:
 *    loc = location for error messages
 *    sc = scope for argument interpretation
 *    args = expressions to print
 * Returns:
 *    `true` on success
 */
bool pragmaMsgSemantic(Loc loc, Scope* sc, Expressions* args)
{
    if (!args)
        return true;
    foreach (arg; *args)
    {
        sc = sc.startCTFE();
        auto e = arg.expressionSemantic(sc);
        e = resolveProperties(sc, e);
        sc = sc.endCTFE();

        // pragma(msg) is allowed to contain types as well as expressions
        e = ctfeInterpretForPragmaMsg(e);
        if (e.op == EXP.error)
        {
            errorSupplemental(loc, "while evaluating `pragma(msg, %s)`", arg.toChars());
            return false;
        }
        if (auto se = e.toStringExp())
        {
            const slice = se.toUTF8(sc).peekString();
            fprintf(stderr, "%.*s", cast(int)slice.length, slice.ptr);
        }
        else
            fprintf(stderr, "%s", e.toChars());
    }
    fprintf(stderr, "\n");
    return true;
}

/***********************************************************
 * Evaluate `pragma(startAddress, func)` and store the resolved symbol in `args`
 *
 * Params:
 *    loc = location for error messages
 *    sc = scope for argument interpretation
 *    args = pragma arguments
 * Returns:
 *    `true` on success
 */
bool pragmaStartAddressSemantic(Loc loc, Scope* sc, Expressions* args)
{
    if (!args || args.length != 1)
    {
        .error(loc, "function name expected for start address");
        return false;
    }
    else
    {
        /* https://issues.dlang.org/show_bug.cgi?id=11980
         * resolveProperties and ctfeInterpret call are not necessary.
         */
        Expression e = (*args)[0];
        sc = sc.startCTFE();
        e = e.expressionSemantic(sc);
        // e = resolveProperties(sc, e);
        sc = sc.endCTFE();

        // e = e.ctfeInterpret();
        (*args)[0] = e;
        Dsymbol sa = getDsymbol(e);
        if (!sa || !sa.isFuncDeclaration())
        {
            .error(loc, "function name expected for start address, not `%s`", e.toChars());
            return false;
        }
    }
    return true;
}
