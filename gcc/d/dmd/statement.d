/**
 * Defines AST nodes for statements.
 *
 * Specification: $(LINK2 https://dlang.org/spec/statement.html, Statements)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/statement.d, _statement.d)
 * Documentation:  https://dlang.org/phobos/dmd_statement.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/statement.d
 */

module dmd.statement;

import core.stdc.stdarg;
import core.stdc.stdio;

import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.cond;
import dmd.declaration;
import dmd.dsymbol;
import dmd.expression;
import dmd.func;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.rootobject;
import dmd.staticassert;
import dmd.tokens;
import dmd.visitor;
import dmd.visitor.postorder;

/***********************************************************
 * Specification: https://dlang.org/spec/statement.html
 */
extern (C++) abstract class Statement : ASTNode
{
    const Loc loc;
    const STMT stmt;

    override final DYNCAST dyncast() const
    {
        return DYNCAST.statement;
    }

    final extern (D) this(const ref Loc loc, STMT stmt) @safe
    {
        this.loc = loc;
        this.stmt = stmt;
        // If this is an in{} contract scope statement (skip for determining
        //  inlineStatus of a function body for header content)
    }

    Statement syntaxCopy()
    {
        assert(0);
    }

    /*************************************
     * Do syntax copy of an array of Statement's.
     */
    static Statements* arraySyntaxCopy(Statements* a)
    {
        Statements* b = null;
        if (a)
        {
            b = a.copy();
            foreach (i, s; *a)
            {
                (*b)[i] = s ? s.syntaxCopy() : null;
            }
        }
        return b;
    }

    Statement getRelatedLabeled()
    {
        return this;
    }

    /****************************
     * Determine if an enclosed `break` would apply to this
     * statement, such as if it is a loop or switch statement.
     * Returns:
     *     `true` if it does
     */
    bool hasBreak() const pure nothrow
    {
        //printf("Statement::hasBreak()\n");
        return false;
    }

    /****************************
     * Determine if an enclosed `continue` would apply to this
     * statement, such as if it is a loop statement.
     * Returns:
     *     `true` if it does
     */
    bool hasContinue() const pure nothrow
    {
        return false;
    }

    /**********************************
     * Returns:
     *     `true` if statement uses exception handling
     */
    extern (D) final bool usesEH()
    {
        extern (C++) final class UsesEH : StoppableVisitor
        {
            alias visit = typeof(super).visit;
        public:
            override void visit(Statement s)
            {
            }

            override void visit(TryCatchStatement s)
            {
                stop = true;
            }

            override void visit(TryFinallyStatement s)
            {
                stop = true;
            }

            override void visit(ScopeGuardStatement s)
            {
                stop = true;
            }

            override void visit(SynchronizedStatement s)
            {
                stop = true;
            }
        }

        scope UsesEH ueh = new UsesEH();
        return walkPostorder(this, ueh);
    }

    /**********************************
     * Returns:
     *   `true` if statement 'comes from' somewhere else, like a goto
     */
    extern (D) final bool comeFrom()
    {
        extern (C++) final class ComeFrom : StoppableVisitor
        {
            alias visit = typeof(super).visit;
        public:
            override void visit(Statement s)
            {
            }

            override void visit(CaseStatement s)
            {
                stop = true;
            }

            override void visit(DefaultStatement s)
            {
                stop = true;
            }

            override void visit(LabelStatement s)
            {
                stop = true;
            }

            override void visit(AsmStatement s)
            {
                stop = true;
            }
        }

        scope ComeFrom cf = new ComeFrom();
        return walkPostorder(this, cf);
    }

    /**********************************
     * Returns:
     *   `true` if statement has executable code.
     */
    extern (D) final bool hasCode()
    {
        extern (C++) final class HasCode : StoppableVisitor
        {
            alias visit = typeof(super).visit;
        public:
            override void visit(Statement s)
            {
                stop = true;
            }

            override void visit(ExpStatement s)
            {
                if (s.exp !is null)
                {
                    stop = s.exp.hasCode();
                }
            }

            override void visit(CompoundStatement s)
            {
            }

            override void visit(ScopeStatement s)
            {
            }

            override void visit(ImportStatement s)
            {
            }

            override void visit(CaseStatement s)
            {
            }

            override void visit(DefaultStatement s)
            {
            }

            override void visit(LabelStatement s)
            {
            }
        }

        scope HasCode hc = new HasCode();
        return walkPostorder(this, hc);
    }

    /*******************************
     * Find last statement in a sequence of statements.
     * Returns:
     *  the last statement, or `null` if there isn't one
     */
    inout(Statement) last() inout nothrow pure
    {
        return this;
    }

    /**************************
     * Support Visitor Pattern
     * Params:
     *  v = visitor
     */
    override void accept(Visitor v)
    {
        v.visit(this);
    }

    /************************************
     * Does this statement end with a return statement?
     *
     * I.e. is it a single return statement or some compound statement
     * that unconditionally hits a return statement.
     * Returns:
     *  return statement it ends with, otherwise null
     */
    pure nothrow @nogc
    inout(ReturnStatement) endsWithReturnStatement() inout { return null; }

    final pure inout nothrow @nogc @trusted
    {
        /********************
         * A cheaper method of doing downcasting of Statements.
         * Returns:
         *    the downcast statement if it can be downcasted, otherwise `null`
         */
        inout(ErrorStatement)       isErrorStatement()       { return stmt == STMT.Error       ? cast(typeof(return))this : null; }
        inout(PeelStatement)        isPeelStatement()        { return stmt == STMT.Peel        ? cast(typeof(return))this : null; }
        inout(ScopeStatement)       isScopeStatement()       { return stmt == STMT.Scope       ? cast(typeof(return))this : null; }
        inout(ExpStatement)         isExpStatement()         { return stmt == STMT.Exp         ? cast(typeof(return))this : null; }
        inout(CompoundStatement)    isCompoundStatement()    { return stmt == STMT.Compound    ? cast(typeof(return))this : null; }
        inout(ReturnStatement)      isReturnStatement()      { return stmt == STMT.Return      ? cast(typeof(return))this : null; }
        inout(IfStatement)          isIfStatement()          { return stmt == STMT.If          ? cast(typeof(return))this : null; }
        inout(ConditionalStatement) isConditionalStatement() { return stmt == STMT.Conditional ? cast(typeof(return))this : null; }
        inout(StaticForeachStatement) isStaticForeachStatement() { return stmt == STMT.StaticForeach ? cast(typeof(return))this : null; }
        inout(CaseStatement)        isCaseStatement()        { return stmt == STMT.Case        ? cast(typeof(return))this : null; }
        inout(DefaultStatement)     isDefaultStatement()     { return stmt == STMT.Default     ? cast(typeof(return))this : null; }
        inout(LabelStatement)       isLabelStatement()       { return stmt == STMT.Label       ? cast(typeof(return))this : null; }
        inout(GotoStatement)        isGotoStatement()        { return stmt == STMT.Goto        ? cast(typeof(return))this : null; }
        inout(GotoDefaultStatement) isGotoDefaultStatement() { return stmt == STMT.GotoDefault ? cast(typeof(return))this : null; }
        inout(GotoCaseStatement)    isGotoCaseStatement()    { return stmt == STMT.GotoCase    ? cast(typeof(return))this : null; }
        inout(BreakStatement)       isBreakStatement()       { return stmt == STMT.Break       ? cast(typeof(return))this : null; }
        inout(DtorExpStatement)     isDtorExpStatement()     { return stmt == STMT.DtorExp     ? cast(typeof(return))this : null; }
        inout(MixinStatement)       isMixinStatement()       { return stmt == STMT.Mixin       ? cast(typeof(return))this : null; }
        inout(ForwardingStatement)  isForwardingStatement()  { return stmt == STMT.Forwarding  ? cast(typeof(return))this : null; }
        inout(DoStatement)          isDoStatement()          { return stmt == STMT.Do          ? cast(typeof(return))this : null; }
        inout(WhileStatement)       isWhileStatement()       { return stmt == STMT.While       ? cast(typeof(return))this : null; }
        inout(ForStatement)         isForStatement()         { return stmt == STMT.For         ? cast(typeof(return))this : null; }
        inout(ForeachStatement)     isForeachStatement()     { return stmt == STMT.Foreach     ? cast(typeof(return))this : null; }
        inout(SwitchStatement)      isSwitchStatement()      { return stmt == STMT.Switch      ? cast(typeof(return))this : null; }
        inout(ContinueStatement)    isContinueStatement()    { return stmt == STMT.Continue    ? cast(typeof(return))this : null; }
        inout(WithStatement)        isWithStatement()        { return stmt == STMT.With        ? cast(typeof(return))this : null; }
        inout(TryCatchStatement)    isTryCatchStatement()    { return stmt == STMT.TryCatch    ? cast(typeof(return))this : null; }
        inout(ThrowStatement)       isThrowStatement()       { return stmt == STMT.Throw       ? cast(typeof(return))this : null; }
        inout(DebugStatement)       isDebugStatement()       { return stmt == STMT.Debug       ? cast(typeof(return))this : null; }
        inout(TryFinallyStatement)  isTryFinallyStatement()  { return stmt == STMT.TryFinally  ? cast(typeof(return))this : null; }
        inout(ScopeGuardStatement)  isScopeGuardStatement()  { return stmt == STMT.ScopeGuard  ? cast(typeof(return))this : null; }
        inout(SwitchErrorStatement)  isSwitchErrorStatement()  { return stmt == STMT.SwitchError  ? cast(typeof(return))this : null; }
        inout(UnrolledLoopStatement) isUnrolledLoopStatement() { return stmt == STMT.UnrolledLoop ? cast(typeof(return))this : null; }
        inout(ForeachRangeStatement) isForeachRangeStatement() { return stmt == STMT.ForeachRange ? cast(typeof(return))this : null; }
        inout(CompoundDeclarationStatement) isCompoundDeclarationStatement() { return stmt == STMT.CompoundDeclaration ? cast(typeof(return))this : null; }
        inout(CompoundAsmStatement)  isCompoundAsmStatement()  { return stmt == STMT.CompoundAsm  ? cast(typeof(return))this : null; }
        inout(PragmaStatement)       isPragmaStatement()       { return stmt == STMT.Pragma       ? cast(typeof(return))this : null; }
        inout(StaticAssertStatement) isStaticAssertStatement() { return stmt == STMT.StaticAssert ? cast(typeof(return))this : null; }
        inout(CaseRangeStatement)    isCaseRangeStatement()    { return stmt == STMT.CaseRange    ? cast(typeof(return))this : null; }
        inout(SynchronizedStatement) isSynchronizedStatement() { return stmt == STMT.Synchronized ? cast(typeof(return))this : null; }
        inout(AsmStatement)          isAsmStatement()          { return stmt == STMT.Asm          ? cast(typeof(return))this : null; }
        inout(InlineAsmStatement)    isInlineAsmStatement()    { return stmt == STMT.InlineAsm    ? cast(typeof(return))this : null; }
        inout(GccAsmStatement)       isGccAsmStatement()       { return stmt == STMT.GccAsm       ? cast(typeof(return))this : null; }
        inout(ImportStatement)       isImportStatement()       { return stmt == STMT.Import       ? cast(typeof(return))this : null; }
    }
}

/***********************************************************
 * Any Statement that fails semantic() or has a component that is an ErrorExp or
 * a TypeError should return an ErrorStatement from semantic().
 */
extern (C++) final class ErrorStatement : Statement
{
    extern (D) this()
    {
        super(Loc.initial, STMT.Error);

        import dmd.globals;
        assert(global.gaggedErrors || global.errors);
    }

    override ErrorStatement syntaxCopy()
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
extern (C++) final class PeelStatement : Statement
{
    Statement s;

    extern (D) this(Statement s) @safe
    {
        super(s.loc, STMT.Peel);
        this.s = s;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}


/***********************************************************
 * https://dlang.org/spec/statement.html#ExpressionStatement
 */
extern (C++) class ExpStatement : Statement
{
    Expression exp;

    final extern (D) this(const ref Loc loc, Expression exp) @safe
    {
        super(loc, STMT.Exp);
        this.exp = exp;
    }

    final extern (D) this(const ref Loc loc, Expression exp, STMT stmt) @safe
    {
        super(loc, stmt);
        this.exp = exp;
    }

    final extern (D) this(const ref Loc loc, Dsymbol declaration) @safe
    {
        super(loc, STMT.Exp);
        this.exp = new DeclarationExp(loc, declaration);
    }

    static ExpStatement create(const ref Loc loc, Expression exp) @safe
    {
        return new ExpStatement(loc, exp);
    }

    override ExpStatement syntaxCopy()
    {
        return new ExpStatement(loc, exp ? exp.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DtorExpStatement : ExpStatement
{
    // Wraps an expression that is the destruction of 'var'
    VarDeclaration var;

    extern (D) this(const ref Loc loc, Expression exp, VarDeclaration var) @safe
    {
        super(loc, exp, STMT.DtorExp);
        this.var = var;
    }

    override DtorExpStatement syntaxCopy()
    {
        return new DtorExpStatement(loc, exp ? exp.syntaxCopy() : null, var);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#mixin-statement
 */
// Note: was called CompileStatement
extern (C++) final class MixinStatement : Statement
{
    Expressions* exps;

    extern (D) this(const ref Loc loc, Expression exp)
    {
        Expressions* exps = new Expressions();
        exps.push(exp);
        this(loc, exps);
    }

    extern (D) this(const ref Loc loc, Expressions* exps) @safe
    {
        super(loc, STMT.Mixin);
        this.exps = exps;
    }

    override MixinStatement syntaxCopy()
    {
        return new MixinStatement(loc, Expression.arraySyntaxCopy(exps));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class CompoundStatement : Statement
{
    Statements* statements;

    /**
     * Construct a `CompoundStatement` using an already existing
     * array of `Statement`s
     *
     * Params:
     *   loc = Instantiation information
     *   statements   = An array of `Statement`s, that will referenced by this class
     */
    final extern (D) this(const ref Loc loc, Statements* statements) @safe
    {
        super(loc, STMT.Compound);
        this.statements = statements;
    }

    final extern (D) this(const ref Loc loc, Statements* statements, STMT stmt) @safe
    {
        super(loc, stmt);
        this.statements = statements;
    }

    /**
     * Construct a `CompoundStatement` from an array of `Statement`s
     *
     * Params:
     *   loc = Instantiation information
     *   sts   = A variadic array of `Statement`s, that will copied in this class
     *         The entries themselves will not be copied.
     */
    final extern (D) this(const ref Loc loc, Statement[] sts...)
    {
        super(loc, STMT.Compound);
        statements = new Statements();
        statements.reserve(sts.length);
        foreach (s; sts)
            statements.push(s);
    }

    static CompoundStatement create(const ref Loc loc, Statement s1, Statement s2)
    {
        return new CompoundStatement(loc, s1, s2);
    }

    override CompoundStatement syntaxCopy()
    {
        return new CompoundStatement(loc, Statement.arraySyntaxCopy(statements));
    }

    override final inout(ReturnStatement) endsWithReturnStatement() inout nothrow pure
    {
        foreach (s; *statements)
        {
            if (s)
            {
                if (inout rs = s.endsWithReturnStatement())
                    return rs;
            }
        }
        return null;
    }

    override final inout(Statement) last() inout nothrow pure
    {
        Statement s = null;
        for (size_t i = statements.length; i; --i)
        {
            s = cast(Statement)(*statements)[i - 1];
            if (s)
            {
                s = cast(Statement)s.last();
                if (s)
                    break;
            }
        }
        return cast(inout)s;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class CompoundDeclarationStatement : CompoundStatement
{
    extern (D) this(const ref Loc loc, Statements* statements) @safe
    {
        super(loc, statements, STMT.CompoundDeclaration);
    }

    override CompoundDeclarationStatement syntaxCopy()
    {
        return new CompoundDeclarationStatement(loc, Statement.arraySyntaxCopy(statements));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The purpose of this is so that continue will go to the next
 * of the statements, and break will go to the end of the statements.
 */
extern (C++) final class UnrolledLoopStatement : Statement
{
    Statements* statements;

    extern (D) this(const ref Loc loc, Statements* statements) @safe
    {
        super(loc, STMT.UnrolledLoop);
        this.statements = statements;
    }

    override UnrolledLoopStatement syntaxCopy()
    {
        return new UnrolledLoopStatement(loc, Statement.arraySyntaxCopy(statements));
    }

    override bool hasBreak() const pure nothrow
    {
        return true;
    }

    override bool hasContinue() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class ScopeStatement : Statement
{
    Statement statement;
    Loc endloc;                 // location of closing curly bracket

    extern (D) this(const ref Loc loc, Statement statement, Loc endloc) @safe
    {
        super(loc, STMT.Scope);
        this.statement = statement;
        this.endloc = endloc;
    }

    override ScopeStatement syntaxCopy()
    {
        return new ScopeStatement(loc, statement ? statement.syntaxCopy() : null, endloc);
    }

    override inout(ReturnStatement) endsWithReturnStatement() inout nothrow pure
    {
        if (statement)
            return statement.endsWithReturnStatement();
        return null;
    }

    override bool hasBreak() const pure nothrow
    {
        //printf("ScopeStatement::hasBreak() %s\n", toChars());
        return statement ? statement.hasBreak() : false;
    }

    override bool hasContinue() const pure nothrow
    {
        return statement ? statement.hasContinue() : false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Statement whose symbol table contains foreach index variables in a
 * local scope and forwards other members to the parent scope.  This
 * wraps a statement.
 *
 * Also see: `dmd.attrib.ForwardingAttribDeclaration`
 */
extern (C++) final class ForwardingStatement : Statement
{
    /// The symbol containing the `static foreach` variables.
    ForwardingScopeDsymbol sym = null;
    /// The wrapped statement.
    Statement statement;

    extern (D) this(const ref Loc loc, ForwardingScopeDsymbol sym, Statement statement) @safe
    {
        super(loc, STMT.Forwarding);
        this.sym = sym;
        assert(statement);
        this.statement = statement;
    }

    extern (D) this(const ref Loc loc, Statement statement) @safe
    {
        auto sym = new ForwardingScopeDsymbol();
        sym.symtab = new DsymbolTable();
        this(loc, sym, statement);
    }

    override ForwardingStatement syntaxCopy()
    {
        return new ForwardingStatement(loc, statement.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}


/***********************************************************
 * https://dlang.org/spec/statement.html#while-statement
 */
extern (C++) final class WhileStatement : Statement
{
    Parameter param;
    Expression condition;
    Statement _body;
    Loc endloc;             // location of closing curly bracket

    extern (D) this(const ref Loc loc, Expression condition, Statement _body, Loc endloc, Parameter param = null) @safe
    {
        super(loc, STMT.While);
        this.condition = condition;
        this._body = _body;
        this.endloc = endloc;
        this.param = param;
    }

    override WhileStatement syntaxCopy()
    {
        return new WhileStatement(loc,
            condition.syntaxCopy(),
            _body ? _body.syntaxCopy() : null,
            endloc, param ? param.syntaxCopy() : null);
    }

    override bool hasBreak() const pure nothrow
    {
        return true;
    }

    override bool hasContinue() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#do-statement
 */
extern (C++) final class DoStatement : Statement
{
    Statement _body;
    Expression condition;
    Loc endloc;                 // location of ';' after while

    extern (D) this(const ref Loc loc, Statement _body, Expression condition, Loc endloc) @safe
    {
        super(loc, STMT.Do);
        this._body = _body;
        this.condition = condition;
        this.endloc = endloc;
    }

    override DoStatement syntaxCopy()
    {
        return new DoStatement(loc,
            _body ? _body.syntaxCopy() : null,
            condition.syntaxCopy(),
            endloc);
    }

    override bool hasBreak() const pure nothrow
    {
        return true;
    }

    override bool hasContinue() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#for-statement
 */
extern (C++) final class ForStatement : Statement
{
    Statement _init;
    Expression condition;
    Expression increment;
    Statement _body;
    Loc endloc;             // location of closing curly bracket

    // When wrapped in try/finally clauses, this points to the outermost one,
    // which may have an associated label. Internal break/continue statements
    // treat that label as referring to this loop.
    Statement relatedLabeled;

    extern (D) this(const ref Loc loc, Statement _init, Expression condition, Expression increment, Statement _body, Loc endloc) @safe
    {
        super(loc, STMT.For);
        this._init = _init;
        this.condition = condition;
        this.increment = increment;
        this._body = _body;
        this.endloc = endloc;
    }

    override ForStatement syntaxCopy()
    {
        return new ForStatement(loc,
            _init ? _init.syntaxCopy() : null,
            condition ? condition.syntaxCopy() : null,
            increment ? increment.syntaxCopy() : null,
            _body.syntaxCopy(),
            endloc);
    }

    override Statement getRelatedLabeled()
    {
        return relatedLabeled ? relatedLabeled : this;
    }

    override bool hasBreak() const pure nothrow
    {
        //printf("ForStatement::hasBreak()\n");
        return true;
    }

    override bool hasContinue() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#foreach-statement
 */
extern (C++) final class ForeachStatement : Statement
{
    TOK op;                     // TOK.foreach_ or TOK.foreach_reverse_
    Parameters* parameters;     // array of Parameters, one for each ForeachType
    Expression aggr;            // ForeachAggregate
    Statement _body;            // NoScopeNonEmptyStatement
    Loc endloc;                 // location of closing curly bracket

    VarDeclaration key;
    VarDeclaration value;

    FuncDeclaration func;       // function we're lexically in

    Statements* cases;          // put breaks, continues, gotos and returns here
    ScopeStatements* gotos;     // forward referenced goto's go here

    extern (D) this(const ref Loc loc, TOK op, Parameters* parameters, Expression aggr, Statement _body, Loc endloc) @safe
    {
        super(loc, STMT.Foreach);
        this.op = op;
        this.parameters = parameters;
        this.aggr = aggr;
        this._body = _body;
        this.endloc = endloc;
    }

    override ForeachStatement syntaxCopy()
    {
        return new ForeachStatement(loc, op,
            Parameter.arraySyntaxCopy(parameters),
            aggr.syntaxCopy(),
            _body ? _body.syntaxCopy() : null,
            endloc);
    }

    override bool hasBreak() const pure nothrow
    {
        return true;
    }

    override bool hasContinue() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#foreach-range-statement
 */
extern (C++) final class ForeachRangeStatement : Statement
{
    TOK op;                 // TOK.foreach_ or TOK.foreach_reverse_
    Parameter param;          // loop index variable
    Expression lwr;
    Expression upr;
    Statement _body;
    Loc endloc;             // location of closing curly bracket

    VarDeclaration key;

    extern (D) this(const ref Loc loc, TOK op, Parameter param, Expression lwr, Expression upr, Statement _body, Loc endloc) @safe
    {
        super(loc, STMT.ForeachRange);
        this.op = op;
        this.param = param;
        this.lwr = lwr;
        this.upr = upr;
        this._body = _body;
        this.endloc = endloc;
    }

    override ForeachRangeStatement syntaxCopy()
    {
        return new ForeachRangeStatement(loc, op, param.syntaxCopy(), lwr.syntaxCopy(), upr.syntaxCopy(), _body ? _body.syntaxCopy() : null, endloc);
    }

    override bool hasBreak() const pure nothrow
    {
        return true;
    }

    override bool hasContinue() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#if-statement
 */
extern (C++) final class IfStatement : Statement
{
    Parameter param;
    Expression condition;
    Statement ifbody;
    Statement elsebody;
    VarDeclaration match;   // for MatchExpression results
    Loc endloc;                 // location of closing curly bracket

    extern (D) this(const ref Loc loc, Parameter param, Expression condition, Statement ifbody, Statement elsebody, Loc endloc) @safe
    {
        super(loc, STMT.If);
        this.param = param;
        this.condition = condition;
        this.ifbody = ifbody;
        this.elsebody = elsebody;
        this.endloc = endloc;
    }

    override IfStatement syntaxCopy()
    {
        return new IfStatement(loc,
            param ? param.syntaxCopy() : null,
            condition.syntaxCopy(),
            ifbody ? ifbody.syntaxCopy() : null,
            elsebody ? elsebody.syntaxCopy() : null,
            endloc);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    /******
     * Returns: true if `if (__ctfe)`
     */
    bool isIfCtfeBlock()
    {
        if (auto cv = condition.isVarExp())
            return cv.var.ident == Id.ctfe;
        return false;
    }
}

/***********************************************************
 * https://dlang.org/spec/version.html#ConditionalStatement
 */
extern (C++) final class ConditionalStatement : Statement
{
    Condition condition;
    Statement ifbody;
    Statement elsebody;

    extern (D) this(const ref Loc loc, Condition condition, Statement ifbody, Statement elsebody) @safe
    {
        super(loc, STMT.Conditional);
        this.condition = condition;
        this.ifbody = ifbody;
        this.elsebody = elsebody;
    }

    override ConditionalStatement syntaxCopy()
    {
        return new ConditionalStatement(loc, condition.syntaxCopy(), ifbody.syntaxCopy(), elsebody ? elsebody.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}


/***********************************************************
 * https://dlang.org/spec/version.html#StaticForeachStatement
 * Static foreach statements, like:
 *      void main()
 *      {
 *           static foreach(i; 0 .. 10)
 *           {
 *               pragma(msg, i);
 *           }
 *      }
 */
extern (C++) final class StaticForeachStatement : Statement
{
    StaticForeach sfe;

    extern (D) this(const ref Loc loc, StaticForeach sfe) @safe
    {
        super(loc, STMT.StaticForeach);
        this.sfe = sfe;
    }

    override StaticForeachStatement syntaxCopy()
    {
        return new StaticForeachStatement(loc, sfe.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#pragma-statement
 */
extern (C++) final class PragmaStatement : Statement
{
    const Identifier ident;
    Expressions* args;      // array of Expression's
    Statement _body;

    extern (D) this(const ref Loc loc, const Identifier ident, Expressions* args, Statement _body) @safe
    {
        super(loc, STMT.Pragma);
        this.ident = ident;
        this.args = args;
        this._body = _body;
    }

    override PragmaStatement syntaxCopy()
    {
        return new PragmaStatement(loc, ident, Expression.arraySyntaxCopy(args), _body ? _body.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/version.html#StaticAssert
 */
extern (C++) final class StaticAssertStatement : Statement
{
    StaticAssert sa;

    extern (D) this(StaticAssert sa) @safe
    {
        super(sa.loc, STMT.StaticAssert);
        this.sa = sa;
    }

    override StaticAssertStatement syntaxCopy()
    {
        return new StaticAssertStatement(sa.syntaxCopy(null));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#switch-statement
 */
extern (C++) final class SwitchStatement : Statement
{
    Parameter param;
    Expression condition;           /// switch(condition)
    Statement _body;                ///
    bool isFinal;                   /// https://dlang.org/spec/statement.html#final-switch-statement
    Loc endloc;

    bool hasDefault;                /// true if has default statement
    bool hasVars;                   /// true if has variable case values
    DefaultStatement sdefault;      /// default:
    Statement tryBody;              /// set to TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement tf;         /// set if in the 'finally' block of a TryFinallyStatement
    GotoCaseStatements gotoCases;   /// array of unresolved GotoCaseStatement's
    CaseStatements* cases;          /// array of CaseStatement's
    VarDeclaration lastVar;         /// last observed variable declaration in this statement

    extern (D) this(const ref Loc loc, Parameter param, Expression condition, Statement _body, bool isFinal, Loc endloc)
    {
        super(loc, STMT.Switch);
        this.param = param;
        this.condition = condition;
        this._body = _body;
        this.isFinal = isFinal;
        this.endloc = endloc;
    }

    override SwitchStatement syntaxCopy()
    {
        return new SwitchStatement(loc,
            param ? param.syntaxCopy() : null,
            condition.syntaxCopy(),
            _body.syntaxCopy(),
            isFinal,
            endloc);
    }

    override bool hasBreak() const pure nothrow
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#CaseStatement
 */
extern (C++) final class CaseStatement : Statement
{
    Expression exp;
    Statement statement;

    int index;              // which case it is (since we sort this)
    VarDeclaration lastVar;
    void* extra;            // for use by Statement_toIR()

    extern (D) this(const ref Loc loc, Expression exp, Statement statement) @safe
    {
        super(loc, STMT.Case);
        this.exp = exp;
        this.statement = statement;
    }

    override CaseStatement syntaxCopy()
    {
        return new CaseStatement(loc, exp.syntaxCopy(), statement.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#CaseRangeStatement
 */
extern (C++) final class CaseRangeStatement : Statement
{
    Expression first;
    Expression last;
    Statement statement;

    extern (D) this(const ref Loc loc, Expression first, Expression last, Statement statement) @safe
    {
        super(loc, STMT.CaseRange);
        this.first = first;
        this.last = last;
        this.statement = statement;
    }

    override CaseRangeStatement syntaxCopy()
    {
        return new CaseRangeStatement(loc, first.syntaxCopy(), last.syntaxCopy(), statement.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#DefaultStatement
 */
extern (C++) final class DefaultStatement : Statement
{
    Statement statement;

    VarDeclaration lastVar;

    extern (D) this(const ref Loc loc, Statement statement) @safe
    {
        super(loc, STMT.Default);
        this.statement = statement;
    }

    override DefaultStatement syntaxCopy()
    {
        return new DefaultStatement(loc, statement.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#GotoStatement
 */
extern (C++) final class GotoDefaultStatement : Statement
{
    SwitchStatement sw;

    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, STMT.GotoDefault);
    }

    override GotoDefaultStatement syntaxCopy()
    {
        return new GotoDefaultStatement(loc);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#GotoStatement
 */
extern (C++) final class GotoCaseStatement : Statement
{
    Expression exp;     // null, or which case to goto

    CaseStatement cs;   // case statement it resolves to

    extern (D) this(const ref Loc loc, Expression exp) @safe
    {
        super(loc, STMT.GotoCase);
        this.exp = exp;
    }

    override GotoCaseStatement syntaxCopy()
    {
        return new GotoCaseStatement(loc, exp ? exp.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class SwitchErrorStatement : Statement
{
    Expression exp;

    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, STMT.SwitchError);
    }

    final extern (D) this(const ref Loc loc, Expression exp) @safe
    {
        super(loc, STMT.SwitchError);
        this.exp = exp;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#return-statement
 */
extern (C++) final class ReturnStatement : Statement
{
    Expression exp;
    size_t caseDim;

    extern (D) this(const ref Loc loc, Expression exp) @safe
    {
        super(loc, STMT.Return);
        this.exp = exp;
    }

    override ReturnStatement syntaxCopy()
    {
        return new ReturnStatement(loc, exp ? exp.syntaxCopy() : null);
    }

    override inout(ReturnStatement) endsWithReturnStatement() inout nothrow pure
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#break-statement
 */
extern (C++) final class BreakStatement : Statement
{
    Identifier ident;

    extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, STMT.Break);
        this.ident = ident;
    }

    override BreakStatement syntaxCopy()
    {
        return new BreakStatement(loc, ident);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#continue-statement
 */
extern (C++) final class ContinueStatement : Statement
{
    Identifier ident;

    extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, STMT.Continue);
        this.ident = ident;
    }

    override ContinueStatement syntaxCopy()
    {
        return new ContinueStatement(loc, ident);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#SynchronizedStatement
 */
extern (C++) final class SynchronizedStatement : Statement
{
    Expression exp;
    Statement _body;

    extern (D) this(const ref Loc loc, Expression exp, Statement _body) @safe
    {
        super(loc, STMT.Synchronized);
        this.exp = exp;
        this._body = _body;
    }

    override SynchronizedStatement syntaxCopy()
    {
        return new SynchronizedStatement(loc, exp ? exp.syntaxCopy() : null, _body ? _body.syntaxCopy() : null);
    }

    override bool hasBreak() const pure nothrow
    {
        return false; //true;
    }

    override bool hasContinue() const pure nothrow
    {
        return false; //true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#with-statement
 */
extern (C++) final class WithStatement : Statement
{
    Expression exp;
    Statement _body;
    VarDeclaration wthis;
    Loc endloc;

    extern (D) this(const ref Loc loc, Expression exp, Statement _body, Loc endloc) @safe
    {
        super(loc, STMT.With);
        this.exp = exp;
        this._body = _body;
        this.endloc = endloc;
    }

    override WithStatement syntaxCopy()
    {
        return new WithStatement(loc, exp.syntaxCopy(), _body ? _body.syntaxCopy() : null, endloc);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#try-statement
 */
extern (C++) final class TryCatchStatement : Statement
{
    Statement _body;
    Catches* catches;

    Statement tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion

    extern (D) this(const ref Loc loc, Statement _body, Catches* catches) @safe
    {
        super(loc, STMT.TryCatch);
        this._body = _body;
        this.catches = catches;
    }

    override TryCatchStatement syntaxCopy()
    {
        auto a = new Catches(catches.length);
        foreach (i, c; *catches)
        {
            (*a)[i] = c.syntaxCopy();
        }
        return new TryCatchStatement(loc, _body.syntaxCopy(), a);
    }

    override bool hasBreak() const pure nothrow
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#Catch
 */
extern (C++) final class Catch : RootObject
{
    const Loc loc;
    Type type;
    Identifier ident;
    Statement handler;

    VarDeclaration var;
    bool errors;                // set if semantic processing errors

    // was generated by the compiler, wasn't present in source code
    bool internalCatch;

    extern (D) this(const ref Loc loc, Type type, Identifier ident, Statement handler) @safe
    {
        //printf("Catch(%s, loc = %s)\n", id.toChars(), loc.toChars());
        this.loc = loc;
        this.type = type;
        this.ident = ident;
        this.handler = handler;
    }

    Catch syntaxCopy()
    {
        auto c = new Catch(loc, type ? type.syntaxCopy() : getThrowable(), ident, (handler ? handler.syntaxCopy() : null));
        c.internalCatch = internalCatch;
        return c;
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#try-statement
 */
extern (C++) final class TryFinallyStatement : Statement
{
    Statement _body;
    Statement finalbody;

    Statement tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion
    bool bodyFallsThru;  /// true if _body falls through to finally

    extern (D) this(const ref Loc loc, Statement _body, Statement finalbody) @safe
    {
        super(loc, STMT.TryFinally);
        this._body = _body;
        this.finalbody = finalbody;
        this.bodyFallsThru = true;      // assume true until statementSemantic()
    }

    static TryFinallyStatement create(const ref Loc loc, Statement _body, Statement finalbody) @safe
    {
        return new TryFinallyStatement(loc, _body, finalbody);
    }

    override TryFinallyStatement syntaxCopy()
    {
        return new TryFinallyStatement(loc, _body.syntaxCopy(), finalbody.syntaxCopy());
    }

    override bool hasBreak() const pure nothrow
    {
        return false; //true;
    }

    override bool hasContinue() const pure nothrow
    {
        return false; //true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#scope-guard-statement
 */
extern (C++) final class ScopeGuardStatement : Statement
{
    TOK tok;
    Statement statement;

    extern (D) this(const ref Loc loc, TOK tok, Statement statement) @safe
    {
        super(loc, STMT.ScopeGuard);
        this.tok = tok;
        this.statement = statement;
    }

    override ScopeGuardStatement syntaxCopy()
    {
        return new ScopeGuardStatement(loc, tok, statement.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#throw-statement
 */
extern (C++) final class ThrowStatement : Statement
{
    Expression exp;

    // was generated by the compiler, wasn't present in source code
    bool internalThrow;

    extern (D) this(const ref Loc loc, Expression exp) @safe
    {
        super(loc, STMT.Throw);
        this.exp = exp;
    }

    override ThrowStatement syntaxCopy()
    {
        auto s = new ThrowStatement(loc, exp.syntaxCopy());
        s.internalThrow = internalThrow;
        return s;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DebugStatement : Statement
{
    Statement statement;

    extern (D) this(const ref Loc loc, Statement statement) @safe
    {
        super(loc, STMT.Debug);
        this.statement = statement;
    }

    override DebugStatement syntaxCopy()
    {
        return new DebugStatement(loc, statement ? statement.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#goto-statement
 */
extern (C++) final class GotoStatement : Statement
{
    Identifier ident;
    LabelDsymbol label;
    Statement tryBody;              /// set to TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement tf;
    ScopeGuardStatement os;
    VarDeclaration lastVar;
    bool inCtfeBlock;               /// set if goto is inside an `if (__ctfe)` block

    extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, STMT.Goto);
        this.ident = ident;
    }

    override GotoStatement syntaxCopy()
    {
        return new GotoStatement(loc, ident);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#LabeledStatement
 */
extern (C++) final class LabelStatement : Statement
{
    Identifier ident;
    Statement statement;

    Statement tryBody;              /// set to TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement tf;
    ScopeGuardStatement os;
    VarDeclaration lastVar;
    Statement gotoTarget;       // interpret
    void* extra;                // used by Statement_toIR()
    bool breaks;                // someone did a 'break ident'
    bool inCtfeBlock;           // inside a block dominated by `if (__ctfe)`

    extern (D) this(const ref Loc loc, Identifier ident, Statement statement) @safe
    {
        super(loc, STMT.Label);
        this.ident = ident;
        this.statement = statement;
    }

    override LabelStatement syntaxCopy()
    {
        return new LabelStatement(loc, ident, statement ? statement.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class LabelDsymbol : Dsymbol
{
    LabelStatement statement;

    bool deleted;           // set if rewritten to return in foreach delegate
    bool iasm;              // set if used by inline assembler

    // set if label was defined multiple times, to avoid duplicate errors
    // can be removed if generic error message deduplication is implemented
    bool duplicated;

    extern (D) this(Identifier ident, const ref Loc loc = Loc.initial) @safe
    {
        super(loc, ident);
    }

    static LabelDsymbol create(Identifier ident) @safe
    {
        return new LabelDsymbol(ident);
    }

    // is this a LabelDsymbol()?
    override LabelDsymbol isLabel()
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/statement.html#asm
 */
extern (C++) class AsmStatement : Statement
{
    Token* tokens;
    bool caseSensitive;  // for register names

    extern (D) this(const ref Loc loc, Token* tokens) @safe
    {
        super(loc, STMT.Asm);
        this.tokens = tokens;
    }

    extern (D) this(const ref Loc loc, Token* tokens, STMT stmt) @safe
    {
        super(loc, stmt);
        this.tokens = tokens;
    }

    override AsmStatement syntaxCopy()
    {
        return new AsmStatement(loc, tokens);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/iasm.html
 */
extern (C++) final class InlineAsmStatement : AsmStatement
{
    void* asmcode;
    uint asmalign;  // alignment of this statement
    uint regs;      // mask of registers modified (must match regm_t in back end)
    bool refparam;  // true if function parameter is referenced
    bool naked;     // true if function is to be naked

    extern (D) this(const ref Loc loc, Token* tokens) @safe
    {
        super(loc, tokens, STMT.InlineAsm);
    }

    override InlineAsmStatement syntaxCopy()
    {
        return new InlineAsmStatement(loc, tokens);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html
 * Assembler instructions with D expression operands.
 */
extern (C++) final class GccAsmStatement : AsmStatement
{
    StorageClass stc;           // attributes of the asm {} block
    Expression insn;            // string expression that is the template for assembler code
    Expressions* args;          // input and output operands of the statement
    uint outputargs;            // of the operands in 'args', the number of output operands
    Identifiers* names;         // list of symbolic names for the operands
    Expressions* constraints;   // list of string constants specifying constraints on operands
    Expressions* clobbers;      // list of string constants specifying clobbers and scratch registers
    Identifiers* labels;        // list of goto labels
    GotoStatements* gotos;      // of the goto labels, the equivalent statements they represent

    extern (D) this(const ref Loc loc, Token* tokens) @safe
    {
        super(loc, tokens, STMT.GccAsm);
    }

    override GccAsmStatement syntaxCopy()
    {
        return new GccAsmStatement(loc, tokens);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * a complete asm {} block
 */
extern (C++) final class CompoundAsmStatement : CompoundStatement
{
    StorageClass stc; // postfix attributes like nothrow/pure/@trusted

    extern (D) this(const ref Loc loc, Statements* statements, StorageClass stc) @safe
    {
        super(loc, statements, STMT.CompoundAsm);
        this.stc = stc;
    }

    override CompoundAsmStatement syntaxCopy()
    {
        return new CompoundAsmStatement(loc, Statement.arraySyntaxCopy(statements), stc);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/module.html#ImportDeclaration
 */
extern (C++) final class ImportStatement : Statement
{
    Dsymbols* imports;      // Array of Import's

    extern (D) this(const ref Loc loc, Dsymbols* imports) @safe
    {
        super(loc, STMT.Import);
        this.imports = imports;
    }

    override ImportStatement syntaxCopy()
    {
        auto m = new Dsymbols(imports.length);
        foreach (i, s; *imports)
        {
            (*m)[i] = s.syntaxCopy(null);
        }
        return new ImportStatement(loc, m);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}


mixin template VisitStatement(Result)
{
    Result VisitStatement(Statement s)
    {
        final switch (s.stmt)
        {
            case STMT.Error:         mixin(visitStmtCase("Error"));
            case STMT.Scope:         mixin(visitStmtCase("Scope"));
            case STMT.Exp:           mixin(visitStmtCase("Exp"));
            case STMT.Compound:      mixin(visitStmtCase("Compound"));
            case STMT.Return:        mixin(visitStmtCase("Return"));
            case STMT.If:            mixin(visitStmtCase("If"));
            case STMT.Conditional:   mixin(visitStmtCase("Conditional"));
            case STMT.StaticForeach: mixin(visitStmtCase("StaticForeach"));
            case STMT.Case:          mixin(visitStmtCase("Case"));
            case STMT.Default:       mixin(visitStmtCase("Default"));
            case STMT.Label:         mixin(visitStmtCase("Label"));
            case STMT.Goto:          mixin(visitStmtCase("Goto"));
            case STMT.GotoDefault:   mixin(visitStmtCase("GotoDefault"));
            case STMT.GotoCase:      mixin(visitStmtCase("GotoCase"));
            case STMT.Break:         mixin(visitStmtCase("Break"));
            case STMT.DtorExp:       mixin(visitStmtCase("DtorExp"));
            case STMT.Mixin:         mixin(visitStmtCase("Mixin"));
            case STMT.Forwarding:    mixin(visitStmtCase("Forwarding"));
            case STMT.Do:            mixin(visitStmtCase("Do"));
            case STMT.While:         mixin(visitStmtCase("While"));
            case STMT.For:           mixin(visitStmtCase("For"));
            case STMT.Foreach:       mixin(visitStmtCase("Foreach"));
            case STMT.Switch:        mixin(visitStmtCase("Switch"));
            case STMT.Continue:      mixin(visitStmtCase("Continue"));
            case STMT.With:          mixin(visitStmtCase("With"));
            case STMT.TryCatch:      mixin(visitStmtCase("TryCatch"));
            case STMT.Throw:         mixin(visitStmtCase("Throw"));
            case STMT.Debug:         mixin(visitStmtCase("Debug"));
            case STMT.TryFinally:    mixin(visitStmtCase("TryFinally"));
            case STMT.ScopeGuard:    mixin(visitStmtCase("ScopeGuard"));
            case STMT.SwitchError:   mixin(visitStmtCase("SwitchError"));
            case STMT.UnrolledLoop:  mixin(visitStmtCase("UnrolledLoop"));
            case STMT.ForeachRange:  mixin(visitStmtCase("ForeachRange"));
            case STMT.CompoundDeclaration: mixin(visitStmtCase("CompoundDeclaration"));
            case STMT.Peel:          mixin(visitStmtCase("Peel"));
            case STMT.CompoundAsm:   mixin(visitStmtCase("CompoundAsm"));
            case STMT.Pragma:        mixin(visitStmtCase("Pragma"));
            case STMT.StaticAssert:  mixin(visitStmtCase("StaticAssert"));
            case STMT.CaseRange:     mixin(visitStmtCase("CaseRange"));
            case STMT.Synchronized:  mixin(visitStmtCase("Synchronized"));
            case STMT.Asm:           mixin(visitStmtCase("Asm"));
            case STMT.InlineAsm:     mixin(visitStmtCase("InlineAsm"));
            case STMT.GccAsm:        mixin(visitStmtCase("GccAsm"));
            case STMT.Import:        mixin(visitStmtCase("Import"));
        }
    }
}

/****************************************
 * CTFE-only helper function for VisitInitializer.
 * Params:
 *      handler = string for the name of the visit handler
 * Returns: boilerplate code for a case
 */
pure string visitStmtCase(string handler) @safe
{
    if (__ctfe)
    {
        return
            "
            enum isVoid = is(Result == void);
            auto sx = s.is"~handler~"Statement();
            static if (__traits(compiles, visit"~handler~"(sx)))
            {
                static if (isVoid)
                {
                    visit"~handler~"(sx);
                    return;
                }
                else
                {
                    if (Result r = visit"~handler~"(sx))
                        return r;
                    return Result.init;
                }
            }
            else static if (__traits(compiles, visitDefaultCase(s)))
            {
                static if (isVoid)
                {
                    visitDefaultCase(sx);
                    return;
                }
                else
                {
                    if (Result r = visitDefaultCase(s))
                        return r;
                    return Result.init;
                }
            }
            else
                static assert(0, "~handler~");
            ";
    }
    assert(0);
}
