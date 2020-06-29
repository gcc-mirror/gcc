
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/statement.c
 */

#include "root/dsystem.h"

#include "statement.h"
#include "errors.h"
#include "expression.h"
#include "cond.h"
#include "init.h"
#include "staticassert.h"
#include "scope.h"
#include "declaration.h"
#include "aggregate.h"
#include "id.h"
#include "hdrgen.h"
#include "parse.h"
#include "template.h"
#include "attrib.h"
#include "import.h"

bool walkPostorder(Statement *s, StoppableVisitor *v);
StorageClass mergeFuncAttrs(StorageClass s1, FuncDeclaration *f);
bool checkEscapeRef(Scope *sc, Expression *e, bool gag);
VarDeclaration *copyToTemp(StorageClass stc, const char *name, Expression *e);
Expression *semantic(Expression *e, Scope *sc);
StringExp *semanticString(Scope *sc, Expression *exp, const char *s);
Statement *makeTupleForeachStatic(Scope *sc, ForeachStatement *fs, bool needExpansion);

Identifier *fixupLabelName(Scope *sc, Identifier *ident)
{
    unsigned flags = (sc->flags & SCOPEcontract);
    const char *id = ident->toChars();
    if (flags && flags != SCOPEinvariant &&
        !(id[0] == '_' && id[1] == '_'))
    {
        /* CTFE requires FuncDeclaration::labtab for the interpretation.
         * So fixing the label name inside in/out contracts is necessary
         * for the uniqueness in labtab.
         */
        const char *prefix = flags == SCOPErequire ? "__in_" : "__out_";
        OutBuffer buf;
        buf.printf("%s%s", prefix, ident->toChars());

        const char *name = buf.extractChars();
        ident = Identifier::idPool(name);
    }
    return ident;
}

LabelStatement *checkLabeledLoop(Scope *sc, Statement *statement)
{
    if (sc->slabel && sc->slabel->statement == statement)
    {
        return sc->slabel;
    }
    return NULL;
}

/***********************************************************
 * Check an assignment is used as a condition.
 * Intended to be use before the `semantic` call on `e`.
 * Params:
 *  e = condition expression which is not yet run semantic analysis.
 * Returns:
 *  `e` or ErrorExp.
 */
Expression *checkAssignmentAsCondition(Expression *e)
{
    Expression *ec = e;
    while (ec->op == TOKcomma)
        ec = ((CommaExp *)ec)->e2;
    if (ec->op == TOKassign)
    {
        ec->error("assignment cannot be used as a condition, perhaps == was meant?");
        return new ErrorExp();
    }
    return e;
}

/// Return a type identifier reference to 'object.Throwable'
TypeIdentifier *getThrowable()
{
    TypeIdentifier *tid = new TypeIdentifier(Loc(), Id::empty);
    tid->addIdent(Id::object);
    tid->addIdent(Id::Throwable);
    return tid;
}

/******************************** Statement ***************************/

Statement::Statement(Loc loc)
    : loc(loc)
{
    // If this is an in{} contract scope statement (skip for determining
    //  inlineStatus of a function body for header content)
}

Statement *Statement::syntaxCopy()
{
    assert(0);
    return NULL;
}

void Statement::print()
{
    fprintf(stderr, "%s\n", toChars());
    fflush(stderr);
}

const char *Statement::toChars()
{
    HdrGenState hgs;

    OutBuffer buf;
    ::toCBuffer(this, &buf, &hgs);
    return buf.extractChars();
}


void Statement::error(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::verror(loc, format, ap);
    va_end( ap );
}

void Statement::warning(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::vwarning(loc, format, ap);
    va_end( ap );
}

void Statement::deprecation(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::vdeprecation(loc, format, ap);
    va_end( ap );
}

bool Statement::hasBreak()
{
    //printf("Statement::hasBreak()\n");
    return false;
}

bool Statement::hasContinue()
{
    return false;
}

/* ============================================== */
// true if statement uses exception handling

bool Statement::usesEH()
{
    class UsesEH : public StoppableVisitor
    {
    public:
        void visit(Statement *)             {}
        void visit(TryCatchStatement *)     { stop = true; }
        void visit(TryFinallyStatement *)   { stop = true; }
        void visit(ScopeGuardStatement *)      { stop = true; }
        void visit(SynchronizedStatement *) { stop = true; }
    };

    UsesEH ueh;
    return walkPostorder(this, &ueh);
}

/* ============================================== */
// true if statement 'comes from' somewhere else, like a goto

bool Statement::comeFrom()
{
    class ComeFrom : public StoppableVisitor
    {
    public:
        void visit(Statement *)        {}
        void visit(CaseStatement *)    { stop = true; }
        void visit(DefaultStatement *) { stop = true; }
        void visit(LabelStatement *)   { stop = true; }
        void visit(AsmStatement *)     { stop = true; }
    };

    ComeFrom cf;
    return walkPostorder(this, &cf);
}

/* ============================================== */
// Return true if statement has executable code.

bool Statement::hasCode()
{
    class HasCode : public StoppableVisitor
    {
    public:
        void visit(Statement *)
        {
            stop = true;
        }

        void visit(ExpStatement *s)
        {
            if (s->exp != NULL)
            {
                stop = s->exp->hasCode();
            }
        }

        void visit(CompoundStatement *) {}
        void visit(ScopeStatement *)    {}
        void visit(ImportStatement *)   {}
    };

    HasCode hc;
    return walkPostorder(this, &hc);
}

Statement *Statement::last()
{
    return this;
}

/****************************************
 * If this statement has code that needs to run in a finally clause
 * at the end of the current scope, return that code in the form of
 * a Statement.
 * Output:
 *      *sentry         code executed upon entry to the scope
 *      *sexception     code executed upon exit from the scope via exception
 *      *sfinally       code executed in finally block
 */

Statement *Statement::scopeCode(Scope *, Statement **sentry, Statement **sexception, Statement **sfinally)
{
    //printf("Statement::scopeCode()\n");
    //print();
    *sentry = NULL;
    *sexception = NULL;
    *sfinally = NULL;
    return this;
}

/*********************************
 * Flatten out the scope by presenting the statement
 * as an array of statements.
 * Returns NULL if no flattening necessary.
 */

Statements *Statement::flatten(Scope *)
{
    return NULL;
}


/******************************** ErrorStatement ***************************/

ErrorStatement::ErrorStatement()
    : Statement(Loc())
{
    assert(global.gaggedErrors || global.errors);
}

Statement *ErrorStatement::syntaxCopy()
{
    return this;
}

/******************************** PeelStatement ***************************/

PeelStatement::PeelStatement(Statement *s)
    : Statement(s->loc)
{
    this->s = s;
}

/******************************** ExpStatement ***************************/

ExpStatement::ExpStatement(Loc loc, Expression *exp)
    : Statement(loc)
{
    this->exp = exp;
}

ExpStatement::ExpStatement(Loc loc, Dsymbol *declaration)
    : Statement(loc)
{
    this->exp = new DeclarationExp(loc, declaration);
}

ExpStatement *ExpStatement::create(Loc loc, Expression *exp)
{
    return new ExpStatement(loc, exp);
}

Statement *ExpStatement::syntaxCopy()
{
    return new ExpStatement(loc, exp ? exp->syntaxCopy() : NULL);
}

Statement *ExpStatement::scopeCode(Scope *, Statement **sentry, Statement **sexception, Statement **sfinally)
{
    //printf("ExpStatement::scopeCode()\n");
    //print();

    *sentry = NULL;
    *sexception = NULL;
    *sfinally = NULL;

    if (exp)
    {
        if (exp->op == TOKdeclaration)
        {
            DeclarationExp *de = (DeclarationExp *)(exp);
            VarDeclaration *v = de->declaration->isVarDeclaration();
            if (v && !v->isDataseg())
            {
                if (v->needsScopeDtor())
                {
                    //printf("dtor is: "); v->edtor->print();
                    *sfinally = new DtorExpStatement(loc, v->edtor, v);
                    v->storage_class |= STCnodtor; // don't add in dtor again
                }
            }
        }
    }
    return this;
}

/****************************************
 * Convert TemplateMixin members (== Dsymbols) to Statements.
 */
Statement *toStatement(Dsymbol *s)
{
    class ToStmt : public Visitor
    {
    public:
        Statement *result;

        ToStmt()
        {
            this->result = NULL;
        }

        Statement *visitMembers(Loc loc, Dsymbols *a)
        {
            if (!a)
                return NULL;

            Statements *statements = new Statements();
            for (size_t i = 0; i < a->length; i++)
            {
                statements->push(toStatement((*a)[i]));
            }
            return new CompoundStatement(loc, statements);
        }

        void visit(Dsymbol *s)
        {
            ::error(Loc(), "Internal Compiler Error: cannot mixin %s %s\n", s->kind(), s->toChars());
            result = new ErrorStatement();
        }

        void visit(TemplateMixin *tm)
        {
            Statements *a = new Statements();
            for (size_t i = 0; i < tm->members->length; i++)
            {
                Statement *s = toStatement((*tm->members)[i]);
                if (s)
                    a->push(s);
            }
            result = new CompoundStatement(tm->loc, a);
        }

        /* An actual declaration symbol will be converted to DeclarationExp
         * with ExpStatement.
         */
        Statement *declStmt(Dsymbol *s)
        {
            DeclarationExp *de = new DeclarationExp(s->loc, s);
            de->type = Type::tvoid;     // avoid repeated semantic
            return new ExpStatement(s->loc, de);
        }
        void visit(VarDeclaration *d)       { result = declStmt(d); }
        void visit(AggregateDeclaration *d) { result = declStmt(d); }
        void visit(FuncDeclaration *d)      { result = declStmt(d); }
        void visit(EnumDeclaration *d)      { result = declStmt(d); }
        void visit(AliasDeclaration *d)     { result = declStmt(d); }
        void visit(TemplateDeclaration *d)  { result = declStmt(d); }

        /* All attributes have been already picked by the semantic analysis of
         * 'bottom' declarations (function, struct, class, etc).
         * So we don't have to copy them.
         */
        void visit(StorageClassDeclaration *d)  { result = visitMembers(d->loc, d->decl); }
        void visit(DeprecatedDeclaration *d)    { result = visitMembers(d->loc, d->decl); }
        void visit(LinkDeclaration *d)          { result = visitMembers(d->loc, d->decl); }
        void visit(ProtDeclaration *d)          { result = visitMembers(d->loc, d->decl); }
        void visit(AlignDeclaration *d)         { result = visitMembers(d->loc, d->decl); }
        void visit(UserAttributeDeclaration *d) { result = visitMembers(d->loc, d->decl); }
        void visit(ForwardingAttribDeclaration *d) { result = visitMembers(d->loc, d->decl); }

        void visit(StaticAssert *) {}
        void visit(Import *) {}
        void visit(PragmaDeclaration *) {}

        void visit(ConditionalDeclaration *d)
        {
            result = visitMembers(d->loc, d->include(NULL));
        }

        void visit(StaticForeachDeclaration *d)
        {
            assert(d->sfe && !!d->sfe->aggrfe ^ !!d->sfe->rangefe);
            result = visitMembers(d->loc, d->include(NULL));
        }

        void visit(CompileDeclaration *d)
        {
            result = visitMembers(d->loc, d->include(NULL));
        }
    };

    if (!s)
        return NULL;

    ToStmt v;
    s->accept(&v);
    return v.result;
}

Statements *ExpStatement::flatten(Scope *sc)
{
    /* Bugzilla 14243: expand template mixin in statement scope
     * to handle variable destructors.
     */
    if (exp && exp->op == TOKdeclaration)
    {
        Dsymbol *d = ((DeclarationExp *)exp)->declaration;
        if (TemplateMixin *tm = d->isTemplateMixin())
        {
            Expression *e = semantic(exp, sc);
            if (e->op == TOKerror || tm->errors)
            {
                Statements *a = new Statements();
                a->push(new ErrorStatement());
                return a;
            }
            assert(tm->members);

            Statement *s = toStatement(tm);
            Statements *a = new Statements();
            a->push(s);
            return a;
        }
    }
    return NULL;
}

/******************************** DtorExpStatement ***************************/

DtorExpStatement::DtorExpStatement(Loc loc, Expression *exp, VarDeclaration *v)
    : ExpStatement(loc, exp)
{
    this->var = v;
}

Statement *DtorExpStatement::syntaxCopy()
{
    return new DtorExpStatement(loc, exp ? exp->syntaxCopy() : NULL, var);
}

/******************************** CompileStatement ***************************/

CompileStatement::CompileStatement(Loc loc, Expression *exp)
    : Statement(loc)
{
    this->exp = exp;
}

Statement *CompileStatement::syntaxCopy()
{
    return new CompileStatement(loc, exp->syntaxCopy());
}

static Statements *errorStatements()
{
    Statements *a = new Statements();
    a->push(new ErrorStatement());
    return a;
}

Statements *CompileStatement::flatten(Scope *sc)
{
    //printf("CompileStatement::flatten() %s\n", exp->toChars());
    StringExp *se = semanticString(sc, exp, "argument to mixin");
    if (!se)
        return errorStatements();
    se = se->toUTF8(sc);

    unsigned errors = global.errors;
    Parser p(loc, sc->_module, (utf8_t *)se->string, se->len, 0);
    p.nextToken();

    Statements *a = new Statements();
    while (p.token.value != TOKeof)
    {
        Statement *s = p.parseStatement(PSsemi | PScurlyscope);
        if (!s || p.errors)
        {
            assert(!p.errors || global.errors != errors); // make sure we caught all the cases
            return errorStatements();
        }
        a->push(s);
    }
    return a;
}

/******************************** CompoundStatement ***************************/

CompoundStatement::CompoundStatement(Loc loc, Statements *s)
    : Statement(loc)
{
    statements = s;
}

CompoundStatement::CompoundStatement(Loc loc, Statement *s1, Statement *s2)
    : Statement(loc)
{
    statements = new Statements();
    statements->reserve(2);
    statements->push(s1);
    statements->push(s2);
}

CompoundStatement::CompoundStatement(Loc loc, Statement *s1)
    : Statement(loc)
{
    statements = new Statements();
    statements->push(s1);
}

CompoundStatement *CompoundStatement::create(Loc loc, Statement *s1, Statement *s2)
{
    return new CompoundStatement(loc, s1, s2);
}

Statement *CompoundStatement::syntaxCopy()
{
    Statements *a = new Statements();
    a->setDim(statements->length);
    for (size_t i = 0; i < statements->length; i++)
    {
        Statement *s = (*statements)[i];
        (*a)[i] = s ? s->syntaxCopy() : NULL;
    }
    return new CompoundStatement(loc, a);
}

Statements *CompoundStatement::flatten(Scope *)
{
    return statements;
}

ReturnStatement *CompoundStatement::isReturnStatement()
{
    ReturnStatement *rs = NULL;

    for (size_t i = 0; i < statements->length; i++)
    {
        Statement *s = (*statements)[i];
        if (s)
        {
            rs = s->isReturnStatement();
            if (rs)
                break;
        }
    }
    return rs;
}

Statement *CompoundStatement::last()
{
    Statement *s = NULL;

    for (size_t i = statements->length; i; --i)
    {   s = (*statements)[i - 1];
        if (s)
        {
            s = s->last();
            if (s)
                break;
        }
    }
    return s;
}

/******************************** CompoundDeclarationStatement ***************************/

CompoundDeclarationStatement::CompoundDeclarationStatement(Loc loc, Statements *s)
    : CompoundStatement(loc, s)
{
    statements = s;
}

Statement *CompoundDeclarationStatement::syntaxCopy()
{
    Statements *a = new Statements();
    a->setDim(statements->length);
    for (size_t i = 0; i < statements->length; i++)
    {
        Statement *s = (*statements)[i];
        (*a)[i] = s ? s->syntaxCopy() : NULL;
    }
    return new CompoundDeclarationStatement(loc, a);
}

/**************************** UnrolledLoopStatement ***************************/

UnrolledLoopStatement::UnrolledLoopStatement(Loc loc, Statements *s)
    : Statement(loc)
{
    statements = s;
}

Statement *UnrolledLoopStatement::syntaxCopy()
{
    Statements *a = new Statements();
    a->setDim(statements->length);
    for (size_t i = 0; i < statements->length; i++)
    {
        Statement *s = (*statements)[i];
        (*a)[i] = s ? s->syntaxCopy() : NULL;
    }
    return new UnrolledLoopStatement(loc, a);
}

bool UnrolledLoopStatement::hasBreak()
{
    return true;
}

bool UnrolledLoopStatement::hasContinue()
{
    return true;
}

/******************************** ScopeStatement ***************************/

ScopeStatement::ScopeStatement(Loc loc, Statement *s, Loc endloc)
    : Statement(loc)
{
    this->statement = s;
    this->endloc = endloc;
}

Statement *ScopeStatement::syntaxCopy()
{
    return new ScopeStatement(loc, statement ? statement->syntaxCopy() : NULL, endloc);
}

ReturnStatement *ScopeStatement::isReturnStatement()
{
    if (statement)
        return statement->isReturnStatement();
    return NULL;
}

bool ScopeStatement::hasBreak()
{
    //printf("ScopeStatement::hasBreak() %s\n", toChars());
    return statement ? statement->hasBreak() : false;
}

bool ScopeStatement::hasContinue()
{
    return statement ? statement->hasContinue() : false;
}

/******************************** ForwardingStatement **********************/

/* Statement whose symbol table contains foreach index variables in a
 * local scope and forwards other members to the parent scope.  This
 * wraps a statement.
 *
 * Also see: `ddmd.attrib.ForwardingAttribDeclaration`
 */

ForwardingStatement::ForwardingStatement(Loc loc, ForwardingScopeDsymbol *sym, Statement *s)
    : Statement(loc)
{
    this->sym = sym;
    assert(s);
    this->statement = s;
}

ForwardingStatement::ForwardingStatement(Loc loc, Statement *s)
    : Statement(loc)
{
    this->sym = new ForwardingScopeDsymbol(NULL);
    this->sym->symtab = new DsymbolTable();
    assert(s);
    this->statement = s;
}

Statement *ForwardingStatement::syntaxCopy()
{
    return new ForwardingStatement(loc, statement->syntaxCopy());
}

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

Statements *ForwardingStatement::flatten(Scope *sc)
{
    if (!statement)
    {
        return NULL;
    }
    sc = sc->push(sym);
    Statements *a = statement->flatten(sc);
    sc = sc->pop();
    if (!a)
    {
        return a;
    }
    Statements *b = new Statements();
    b->setDim(a->length);
    for (size_t i = 0; i < a->length; i++)
    {
        Statement *s = (*a)[i];
        (*b)[i] = s ? new ForwardingStatement(s->loc, sym, s) : NULL;
    }
    return b;
}

/******************************** WhileStatement ***************************/

WhileStatement::WhileStatement(Loc loc, Expression *c, Statement *b, Loc endloc)
    : Statement(loc)
{
    condition = c;
    _body = b;
    this->endloc = endloc;
}

Statement *WhileStatement::syntaxCopy()
{
    return new WhileStatement(loc,
        condition->syntaxCopy(),
        _body ? _body->syntaxCopy() : NULL,
        endloc);
}

bool WhileStatement::hasBreak()
{
    return true;
}

bool WhileStatement::hasContinue()
{
    return true;
}

/******************************** DoStatement ***************************/

DoStatement::DoStatement(Loc loc, Statement *b, Expression *c, Loc endloc)
    : Statement(loc)
{
    _body = b;
    condition = c;
    this->endloc = endloc;
}

Statement *DoStatement::syntaxCopy()
{
    return new DoStatement(loc,
        _body ? _body->syntaxCopy() : NULL,
        condition->syntaxCopy(),
        endloc);
}

bool DoStatement::hasBreak()
{
    return true;
}

bool DoStatement::hasContinue()
{
    return true;
}

/******************************** ForStatement ***************************/

ForStatement::ForStatement(Loc loc, Statement *init, Expression *condition, Expression *increment, Statement *body, Loc endloc)
    : Statement(loc)
{
    this->_init = init;
    this->condition = condition;
    this->increment = increment;
    this->_body = body;
    this->endloc = endloc;
    this->relatedLabeled = NULL;
}

Statement *ForStatement::syntaxCopy()
{
    return new ForStatement(loc,
        _init ? _init->syntaxCopy() : NULL,
        condition ? condition->syntaxCopy() : NULL,
        increment ? increment->syntaxCopy() : NULL,
        _body->syntaxCopy(),
        endloc);
}

Statement *ForStatement::scopeCode(Scope *sc, Statement **sentry, Statement **sexception, Statement **sfinally)
{
    //printf("ForStatement::scopeCode()\n");
    Statement::scopeCode(sc, sentry, sexception, sfinally);
    return this;
}

bool ForStatement::hasBreak()
{
    //printf("ForStatement::hasBreak()\n");
    return true;
}

bool ForStatement::hasContinue()
{
    return true;
}

/******************************** ForeachStatement ***************************/

ForeachStatement::ForeachStatement(Loc loc, TOK op, Parameters *parameters,
        Expression *aggr, Statement *body, Loc endloc)
    : Statement(loc)
{
    this->op = op;
    this->parameters = parameters;
    this->aggr = aggr;
    this->_body = body;
    this->endloc = endloc;

    this->key = NULL;
    this->value = NULL;

    this->func = NULL;

    this->cases = NULL;
    this->gotos = NULL;
}

Statement *ForeachStatement::syntaxCopy()
{
    return new ForeachStatement(loc, op,
        Parameter::arraySyntaxCopy(parameters),
        aggr->syntaxCopy(),
        _body ? _body->syntaxCopy() : NULL,
        endloc);
}

bool ForeachStatement::checkForArgTypes()
{
    bool result = false;

    for (size_t i = 0; i < parameters->length; i++)
    {
        Parameter *p = (*parameters)[i];
        if (!p->type)
        {
            error("cannot infer type for %s", p->ident->toChars());
            p->type = Type::terror;
            result = true;
        }
    }
    return result;
}

bool ForeachStatement::hasBreak()
{
    return true;
}

bool ForeachStatement::hasContinue()
{
    return true;
}

/**************************** ForeachRangeStatement ***************************/


ForeachRangeStatement::ForeachRangeStatement(Loc loc, TOK op, Parameter *prm,
        Expression *lwr, Expression *upr, Statement *body, Loc endloc)
    : Statement(loc)
{
    this->op = op;
    this->prm = prm;
    this->lwr = lwr;
    this->upr = upr;
    this->_body = body;
    this->endloc = endloc;

    this->key = NULL;
}

Statement *ForeachRangeStatement::syntaxCopy()
{
    return new ForeachRangeStatement(loc, op,
        prm->syntaxCopy(),
        lwr->syntaxCopy(),
        upr->syntaxCopy(),
        _body ? _body->syntaxCopy() : NULL,
        endloc);
}

bool ForeachRangeStatement::hasBreak()
{
    return true;
}

bool ForeachRangeStatement::hasContinue()
{
    return true;
}

/******************************** IfStatement ***************************/

IfStatement::IfStatement(Loc loc, Parameter *prm, Expression *condition, Statement *ifbody, Statement *elsebody, Loc endloc)
    : Statement(loc)
{
    this->prm = prm;
    this->condition = condition;
    this->ifbody = ifbody;
    this->elsebody = elsebody;
    this->endloc = endloc;
    this->match = NULL;
}

Statement *IfStatement::syntaxCopy()
{
    return new IfStatement(loc,
        prm ? prm->syntaxCopy() : NULL,
        condition->syntaxCopy(),
        ifbody ? ifbody->syntaxCopy() : NULL,
        elsebody ? elsebody->syntaxCopy() : NULL,
        endloc);
}

/******************************** ConditionalStatement ***************************/

ConditionalStatement::ConditionalStatement(Loc loc, Condition *condition, Statement *ifbody, Statement *elsebody)
    : Statement(loc)
{
    this->condition = condition;
    this->ifbody = ifbody;
    this->elsebody = elsebody;
}

Statement *ConditionalStatement::syntaxCopy()
{
    return new ConditionalStatement(loc,
        condition->syntaxCopy(),
        ifbody->syntaxCopy(),
        elsebody ? elsebody->syntaxCopy() : NULL);
}

Statements *ConditionalStatement::flatten(Scope *sc)
{
    Statement *s;

    //printf("ConditionalStatement::flatten()\n");
    if (condition->include(sc))
    {
        DebugCondition *dc = condition->isDebugCondition();
        if (dc)
            s = new DebugStatement(loc, ifbody);
        else
            s = ifbody;
    }
    else
        s = elsebody;

    Statements *a = new Statements();
    a->push(s);
    return a;
}

/******************************** StaticForeachStatement ********************/

/* Static foreach statements, like:
 *      void main()
 *      {
 *           static foreach(i; 0 .. 10)
 *           {
 *               pragma(msg, i);
 *           }
 *      }
 */

StaticForeachStatement::StaticForeachStatement(Loc loc, StaticForeach *sfe)
    : Statement(loc)
{
    this->sfe = sfe;
}

Statement *StaticForeachStatement::syntaxCopy()
{
    return new StaticForeachStatement(loc, sfe->syntaxCopy());
}

Statements *StaticForeachStatement::flatten(Scope *sc)
{
    staticForeachPrepare(sfe, sc);
    if (staticForeachReady(sfe))
    {
        Statement *s = makeTupleForeachStatic(sc, sfe->aggrfe, sfe->needExpansion);
        Statements *result = s->flatten(sc);
        if (result)
        {
            return result;
        }
        result = new Statements();
        result->push(s);
        return result;
    }
    else
    {
        Statements *result = new Statements();
        result->push(new ErrorStatement());
        return result;
    }
}

/******************************** PragmaStatement ***************************/

PragmaStatement::PragmaStatement(Loc loc, Identifier *ident, Expressions *args, Statement *body)
    : Statement(loc)
{
    this->ident = ident;
    this->args = args;
    this->_body = body;
}

Statement *PragmaStatement::syntaxCopy()
{
    return new PragmaStatement(loc, ident,
        Expression::arraySyntaxCopy(args),
        _body ? _body->syntaxCopy() : NULL);
}

/******************************** StaticAssertStatement ***************************/

StaticAssertStatement::StaticAssertStatement(StaticAssert *sa)
    : Statement(sa->loc)
{
    this->sa = sa;
}

Statement *StaticAssertStatement::syntaxCopy()
{
    return new StaticAssertStatement((StaticAssert *)sa->syntaxCopy(NULL));
}

/******************************** SwitchStatement ***************************/

SwitchStatement::SwitchStatement(Loc loc, Expression *c, Statement *b, bool isFinal)
    : Statement(loc)
{
    this->condition = c;
    this->_body = b;
    this->isFinal = isFinal;
    sdefault = NULL;
    tf = NULL;
    cases = NULL;
    hasNoDefault = 0;
    hasVars = 0;
    lastVar = NULL;
}

Statement *SwitchStatement::syntaxCopy()
{
    return new SwitchStatement(loc,
        condition->syntaxCopy(),
        _body->syntaxCopy(),
        isFinal);
}

bool SwitchStatement::hasBreak()
{
    return true;
}

static bool checkVar(SwitchStatement *s, VarDeclaration *vd)
{
    if (!vd || vd->isDataseg() || (vd->storage_class & STCmanifest))
        return false;

    VarDeclaration *last = s->lastVar;
    while (last && last != vd)
        last = last->lastVar;
    if (last == vd)
    {
        // All good, the label's scope has no variables
    }
    else if (vd->storage_class & STCexptemp)
    {
        // Lifetime ends at end of expression, so no issue with skipping the statement
    }
    else if (vd->ident == Id::withSym)
    {
        s->deprecation("'switch' skips declaration of 'with' temporary at %s", vd->loc.toChars());
        return true;
    }
    else
    {
        s->deprecation("'switch' skips declaration of variable %s at %s", vd->toPrettyChars(), vd->loc.toChars());
        return true;
    }

    return false;
}

bool SwitchStatement::checkLabel()
{
    const bool error = true;

    if (sdefault && checkVar(this, sdefault->lastVar))
        return !error; // return error once fully deprecated

    for (size_t i = 0; i < cases->length; i++)
    {
        CaseStatement *scase = (*cases)[i];
        if (scase && checkVar(this, scase->lastVar))
            return !error; // return error once fully deprecated
    }
    return !error;
}

/******************************** CaseStatement ***************************/

CaseStatement::CaseStatement(Loc loc, Expression *exp, Statement *s)
    : Statement(loc)
{
    this->exp = exp;
    this->statement = s;
    index = 0;
    lastVar = NULL;
}

Statement *CaseStatement::syntaxCopy()
{
    return new CaseStatement(loc,
        exp->syntaxCopy(),
        statement->syntaxCopy());
}

int CaseStatement::compare(RootObject *obj)
{
    // Sort cases so we can do an efficient lookup
    CaseStatement *cs2 = (CaseStatement *)(obj);

    return exp->compare(cs2->exp);
}

/******************************** CaseRangeStatement ***************************/


CaseRangeStatement::CaseRangeStatement(Loc loc, Expression *first,
        Expression *last, Statement *s)
    : Statement(loc)
{
    this->first = first;
    this->last = last;
    this->statement = s;
}

Statement *CaseRangeStatement::syntaxCopy()
{
    return new CaseRangeStatement(loc,
        first->syntaxCopy(),
        last->syntaxCopy(),
        statement->syntaxCopy());
}

/******************************** DefaultStatement ***************************/

DefaultStatement::DefaultStatement(Loc loc, Statement *s)
    : Statement(loc)
{
    this->statement = s;
    this->lastVar = NULL;
}

Statement *DefaultStatement::syntaxCopy()
{
    return new DefaultStatement(loc, statement->syntaxCopy());
}

/******************************** GotoDefaultStatement ***************************/

GotoDefaultStatement::GotoDefaultStatement(Loc loc)
    : Statement(loc)
{
    sw = NULL;
}

Statement *GotoDefaultStatement::syntaxCopy()
{
    return new GotoDefaultStatement(loc);
}

/******************************** GotoCaseStatement ***************************/

GotoCaseStatement::GotoCaseStatement(Loc loc, Expression *exp)
    : Statement(loc)
{
    cs = NULL;
    this->exp = exp;
}

Statement *GotoCaseStatement::syntaxCopy()
{
    return new GotoCaseStatement(loc, exp ? exp->syntaxCopy() : NULL);
}

/******************************** SwitchErrorStatement ***************************/

SwitchErrorStatement::SwitchErrorStatement(Loc loc)
    : Statement(loc)
{
}

/******************************** ReturnStatement ***************************/

ReturnStatement::ReturnStatement(Loc loc, Expression *exp)
    : Statement(loc)
{
    this->exp = exp;
    this->caseDim = 0;
}

Statement *ReturnStatement::syntaxCopy()
{
    return new ReturnStatement(loc, exp ? exp->syntaxCopy() : NULL);
}

/******************************** BreakStatement ***************************/

BreakStatement::BreakStatement(Loc loc, Identifier *ident)
    : Statement(loc)
{
    this->ident = ident;
}

Statement *BreakStatement::syntaxCopy()
{
    return new BreakStatement(loc, ident);
}

/******************************** ContinueStatement ***************************/

ContinueStatement::ContinueStatement(Loc loc, Identifier *ident)
    : Statement(loc)
{
    this->ident = ident;
}

Statement *ContinueStatement::syntaxCopy()
{
    return new ContinueStatement(loc, ident);
}

/******************************** SynchronizedStatement ***************************/

SynchronizedStatement::SynchronizedStatement(Loc loc, Expression *exp, Statement *body)
    : Statement(loc)
{
    this->exp = exp;
    this->_body = body;
}

Statement *SynchronizedStatement::syntaxCopy()
{
    return new SynchronizedStatement(loc,
        exp ? exp->syntaxCopy() : NULL,
        _body ? _body->syntaxCopy() : NULL);
}

bool SynchronizedStatement::hasBreak()
{
    return false; //true;
}

bool SynchronizedStatement::hasContinue()
{
    return false; //true;
}

/******************************** WithStatement ***************************/

WithStatement::WithStatement(Loc loc, Expression *exp, Statement *body, Loc endloc)
    : Statement(loc)
{
    this->exp = exp;
    this->_body = body;
    this->endloc = endloc;
    wthis = NULL;
}

Statement *WithStatement::syntaxCopy()
{
    return new WithStatement(loc,
        exp->syntaxCopy(),
        _body ? _body->syntaxCopy() : NULL, endloc);
}

/******************************** TryCatchStatement ***************************/

TryCatchStatement::TryCatchStatement(Loc loc, Statement *body, Catches *catches)
    : Statement(loc)
{
    this->_body = body;
    this->catches = catches;
}

Statement *TryCatchStatement::syntaxCopy()
{
    Catches *a = new Catches();
    a->setDim(catches->length);
    for (size_t i = 0; i < a->length; i++)
    {
        Catch *c = (*catches)[i];
        (*a)[i] = c->syntaxCopy();
    }
    return new TryCatchStatement(loc, _body->syntaxCopy(), a);
}

bool TryCatchStatement::hasBreak()
{
    return false;
}

/******************************** Catch ***************************/

Catch::Catch(Loc loc, Type *t, Identifier *id, Statement *handler)
{
    //printf("Catch(%s, loc = %s)\n", id->toChars(), loc.toChars());
    this->loc = loc;
    this->type = t;
    this->ident = id;
    this->handler = handler;
    var = NULL;
    errors = false;
    internalCatch = false;
}

Catch *Catch::syntaxCopy()
{
    Catch *c = new Catch(loc,
        type ? type->syntaxCopy() : getThrowable(),
        ident,
        (handler ? handler->syntaxCopy() : NULL));
    c->internalCatch = internalCatch;
    return c;
}

/****************************** TryFinallyStatement ***************************/

TryFinallyStatement::TryFinallyStatement(Loc loc, Statement *body, Statement *finalbody)
    : Statement(loc)
{
    this->_body = body;
    this->finalbody = finalbody;
}

TryFinallyStatement *TryFinallyStatement::create(Loc loc, Statement *body, Statement *finalbody)
{
    return new TryFinallyStatement(loc, body, finalbody);
}

Statement *TryFinallyStatement::syntaxCopy()
{
    return new TryFinallyStatement(loc,
        _body->syntaxCopy(), finalbody->syntaxCopy());
}

bool TryFinallyStatement::hasBreak()
{
    return false; //true;
}

bool TryFinallyStatement::hasContinue()
{
    return false; //true;
}

/****************************** ScopeGuardStatement ***************************/

ScopeGuardStatement::ScopeGuardStatement(Loc loc, TOK tok, Statement *statement)
    : Statement(loc)
{
    this->tok = tok;
    this->statement = statement;
}

Statement *ScopeGuardStatement::syntaxCopy()
{
    return new ScopeGuardStatement(loc, tok, statement->syntaxCopy());
}

Statement *ScopeGuardStatement::scopeCode(Scope *sc, Statement **sentry, Statement **sexception, Statement **sfinally)
{
    //printf("ScopeGuardStatement::scopeCode()\n");
    //print();
    *sentry = NULL;
    *sexception = NULL;
    *sfinally = NULL;

    Statement *s = new PeelStatement(statement);

    switch (tok)
    {
        case TOKon_scope_exit:
            *sfinally = s;
            break;

        case TOKon_scope_failure:
            *sexception = s;
            break;

        case TOKon_scope_success:
        {
            /* Create:
             *  sentry:   bool x = false;
             *  sexception:    x = true;
             *  sfinally: if (!x) statement;
             */
            VarDeclaration *v = copyToTemp(0, "__os", new IntegerExp(Loc(), 0, Type::tbool));
            v->semantic(sc);
            *sentry = new ExpStatement(loc, v);

            Expression *e = new IntegerExp(Loc(), 1, Type::tbool);
            e = new AssignExp(Loc(), new VarExp(Loc(), v), e);
            *sexception = new ExpStatement(Loc(), e);

            e = new VarExp(Loc(), v);
            e = new NotExp(Loc(), e);
            *sfinally = new IfStatement(Loc(), NULL, e, s, NULL, Loc());

            break;
        }

        default:
            assert(0);
    }
    return NULL;
}

/******************************** ThrowStatement ***************************/

ThrowStatement::ThrowStatement(Loc loc, Expression *exp)
    : Statement(loc)
{
    this->exp = exp;
    this->internalThrow = false;
}

Statement *ThrowStatement::syntaxCopy()
{
    ThrowStatement *s = new ThrowStatement(loc, exp->syntaxCopy());
    s->internalThrow = internalThrow;
    return s;
}

/******************************** DebugStatement **************************/

DebugStatement::DebugStatement(Loc loc, Statement *statement)
    : Statement(loc)
{
    this->statement = statement;
}

Statement *DebugStatement::syntaxCopy()
{
    return new DebugStatement(loc,
        statement ? statement->syntaxCopy() : NULL);
}

Statements *DebugStatement::flatten(Scope *sc)
{
    Statements *a = statement ? statement->flatten(sc) : NULL;
    if (a)
    {
        for (size_t i = 0; i < a->length; i++)
        {   Statement *s = (*a)[i];

            s = new DebugStatement(loc, s);
            (*a)[i] = s;
        }
    }

    return a;
}

/******************************** GotoStatement ***************************/

GotoStatement::GotoStatement(Loc loc, Identifier *ident)
    : Statement(loc)
{
    this->ident = ident;
    this->label = NULL;
    this->tf = NULL;
    this->os = NULL;
    this->lastVar = NULL;
}

Statement *GotoStatement::syntaxCopy()
{
    return new GotoStatement(loc, ident);
}

bool GotoStatement::checkLabel()
{
    if (!label->statement)
    {
        error("label '%s' is undefined", label->toChars());
        return true;
    }

    if (label->statement->os != os)
    {
        if (os && os->tok == TOKon_scope_failure && !label->statement->os)
        {
            // Jump out from scope(failure) block is allowed.
        }
        else
        {
            if (label->statement->os)
                error("cannot goto in to %s block", Token::toChars(label->statement->os->tok));
            else
                error("cannot goto out of %s block", Token::toChars(os->tok));
            return true;
        }
    }

    if (label->statement->tf != tf)
    {
        error("cannot goto in or out of finally block");
        return true;
    }

    VarDeclaration *vd = label->statement->lastVar;
    if (!vd || vd->isDataseg() || (vd->storage_class & STCmanifest))
        return false;

    VarDeclaration *last = lastVar;
    while (last && last != vd)
        last = last->lastVar;
    if (last == vd)
    {
        // All good, the label's scope has no variables
    }
    else if (vd->ident == Id::withSym)
    {
        error("goto skips declaration of with temporary at %s", vd->loc.toChars());
        return true;
    }
    else
    {
        error("goto skips declaration of variable %s at %s", vd->toPrettyChars(), vd->loc.toChars());
        return true;
    }

    return false;
}

/******************************** LabelStatement ***************************/

LabelStatement::LabelStatement(Loc loc, Identifier *ident, Statement *statement)
    : Statement(loc)
{
    this->ident = ident;
    this->statement = statement;
    this->tf = NULL;
    this->os = NULL;
    this->lastVar = NULL;
    this->gotoTarget = NULL;
    this->breaks = false;
}

Statement *LabelStatement::syntaxCopy()
{
    return new LabelStatement(loc, ident, statement ? statement->syntaxCopy() : NULL);
}

Statement *LabelStatement::scopeCode(Scope *sc, Statement **sentry, Statement **sexit, Statement **sfinally)
{
    //printf("LabelStatement::scopeCode()\n");
    if (statement)
        statement = statement->scopeCode(sc, sentry, sexit, sfinally);
    else
    {
        *sentry = NULL;
        *sexit = NULL;
        *sfinally = NULL;
    }
    return this;
}

Statements *LabelStatement::flatten(Scope *sc)
{
    Statements *a = NULL;

    if (statement)
    {
        a = statement->flatten(sc);
        if (a)
        {
            if (!a->length)
            {
                a->push(new ExpStatement(loc, (Expression *)NULL));
            }

            // reuse 'this' LabelStatement
            this->statement = (*a)[0];
            (*a)[0] = this;
        }
    }

    return a;
}

/******************************** LabelDsymbol ***************************/

LabelDsymbol::LabelDsymbol(Identifier *ident)
        : Dsymbol(ident)
{
    statement = NULL;
}

LabelDsymbol *LabelDsymbol::create(Identifier *ident)
{
    return new LabelDsymbol(ident);
}

LabelDsymbol *LabelDsymbol::isLabel()           // is this a LabelDsymbol()?
{
    return this;
}


/************************ AsmStatement ***************************************/

AsmStatement::AsmStatement(Loc loc, Token *tokens)
    : Statement(loc)
{
    this->tokens = tokens;
}

Statement *AsmStatement::syntaxCopy()
{
    return new AsmStatement(loc, tokens);
}


/************************ InlineAsmStatement **********************************/

InlineAsmStatement::InlineAsmStatement(Loc loc, Token *tokens)
    : AsmStatement(loc, tokens)
{
    asmcode = NULL;
    asmalign = 0;
    refparam = false;
    naked = false;
    regs = 0;
}

Statement *InlineAsmStatement::syntaxCopy()
{
    return new InlineAsmStatement(loc, tokens);
}


/************************ GccAsmStatement ***************************************/

GccAsmStatement::GccAsmStatement(Loc loc, Token *tokens)
        : AsmStatement(loc, tokens)
{
    this->stc = STCundefined;
    this->insn = NULL;
    this->args = NULL;
    this->outputargs = 0;
    this->names = NULL;
    this->constraints = NULL;
    this->clobbers = NULL;
    this->labels = NULL;
    this->gotos = NULL;
}

Statement *GccAsmStatement::syntaxCopy()
{
    return new GccAsmStatement(loc, tokens);
}

/************************ CompoundAsmStatement ***************************************/

CompoundAsmStatement::CompoundAsmStatement(Loc loc, Statements *s, StorageClass stc)
    : CompoundStatement(loc, s)
{
    this->stc = stc;
}

CompoundAsmStatement *CompoundAsmStatement::syntaxCopy()
{
    Statements *a = new Statements();
    a->setDim(statements->length);
    for (size_t i = 0; i < statements->length; i++)
    {
        Statement *s = (*statements)[i];
        (*a)[i] = s ? s->syntaxCopy() : NULL;
    }
    return new CompoundAsmStatement(loc, a, stc);
}

Statements *CompoundAsmStatement::flatten(Scope *)
{
    return NULL;
}

/************************ ImportStatement ***************************************/

ImportStatement::ImportStatement(Loc loc, Dsymbols *imports)
    : Statement(loc)
{
    this->imports = imports;
}

Statement *ImportStatement::syntaxCopy()
{
    Dsymbols *m = new Dsymbols();
    m->setDim(imports->length);
    for (size_t i = 0; i < imports->length; i++)
    {
        Dsymbol *s = (*imports)[i];
        (*m)[i] = s->syntaxCopy(NULL);
    }
    return new ImportStatement(loc, m);
}
