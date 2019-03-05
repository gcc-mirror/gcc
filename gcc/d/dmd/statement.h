
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/statement.h
 */

#pragma once

#include "root/root.h"

#include "arraytypes.h"
#include "dsymbol.h"
#include "visitor.h"
#include "tokens.h"

struct OutBuffer;
struct Scope;
class Expression;
class LabelDsymbol;
class Identifier;
class IfStatement;
class ExpStatement;
class DefaultStatement;
class VarDeclaration;
class Condition;
class Module;
struct Token;
class ErrorStatement;
class ReturnStatement;
class CompoundStatement;
class Parameter;
class StaticAssert;
class AsmStatement;
class GotoStatement;
class ScopeStatement;
class TryCatchStatement;
class TryFinallyStatement;
class CaseStatement;
class DefaultStatement;
class LabelStatement;
class StaticForeach;

// Back end
struct code;

bool inferAggregate(ForeachStatement *fes, Scope *sc, Dsymbol *&sapply);
bool inferApplyArgTypes(ForeachStatement *fes, Scope *sc, Dsymbol *&sapply);

/* How a statement exits; this is returned by blockExit()
 */
enum BE
{
    BEnone =     0,
    BEfallthru = 1,
    BEthrow =    2,
    BEreturn =   4,
    BEgoto =     8,
    BEhalt =     0x10,
    BEbreak =    0x20,
    BEcontinue = 0x40,
    BEerrthrow = 0x80,
    BEany = (BEfallthru | BEthrow | BEreturn | BEgoto | BEhalt)
};

class Statement : public RootObject
{
public:
    Loc loc;

    Statement(Loc loc);
    virtual Statement *syntaxCopy();

    void print();
    const char *toChars();

    void error(const char *format, ...);
    void warning(const char *format, ...);
    void deprecation(const char *format, ...);
    virtual Statement *getRelatedLabeled() { return this; }
    virtual bool hasBreak();
    virtual bool hasContinue();
    bool usesEH();
    bool comeFrom();
    bool hasCode();
    virtual Statement *scopeCode(Scope *sc, Statement **sentry, Statement **sexit, Statement **sfinally);
    virtual Statements *flatten(Scope *sc);
    virtual Statement *last();

    // Avoid dynamic_cast
    virtual ErrorStatement *isErrorStatement() { return NULL; }
    virtual ScopeStatement *isScopeStatement() { return NULL; }
    virtual ExpStatement *isExpStatement() { return NULL; }
    virtual CompoundStatement *isCompoundStatement() { return NULL; }
    virtual ReturnStatement *isReturnStatement() { return NULL; }
    virtual IfStatement *isIfStatement() { return NULL; }
    virtual CaseStatement *isCaseStatement() { return NULL; }
    virtual DefaultStatement *isDefaultStatement() { return NULL; }
    virtual LabelStatement *isLabelStatement() { return NULL; }
    virtual GotoDefaultStatement *isGotoDefaultStatement() { return NULL; }
    virtual GotoCaseStatement *isGotoCaseStatement() { return NULL; }
    virtual BreakStatement *isBreakStatement() { return NULL; }
    virtual DtorExpStatement *isDtorExpStatement() { return NULL; }
    virtual ForwardingStatement *isForwardingStatement() { return NULL; }
    virtual void accept(Visitor *v) { v->visit(this); }
};

/** Any Statement that fails semantic() or has a component that is an ErrorExp or
 * a TypeError should return an ErrorStatement from semantic().
 */
class ErrorStatement : public Statement
{
public:
    ErrorStatement();
    Statement *syntaxCopy();

    ErrorStatement *isErrorStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class PeelStatement : public Statement
{
public:
    Statement *s;

    PeelStatement(Statement *s);
    void accept(Visitor *v) { v->visit(this); }
};

class ExpStatement : public Statement
{
public:
    Expression *exp;

    ExpStatement(Loc loc, Expression *exp);
    ExpStatement(Loc loc, Dsymbol *s);
    static ExpStatement *create(Loc loc, Expression *exp);
    Statement *syntaxCopy();
    Statement *scopeCode(Scope *sc, Statement **sentry, Statement **sexit, Statement **sfinally);
    Statements *flatten(Scope *sc);

    ExpStatement *isExpStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class DtorExpStatement : public ExpStatement
{
public:
    /* Wraps an expression that is the destruction of 'var'
     */

    VarDeclaration *var;

    DtorExpStatement(Loc loc, Expression *exp, VarDeclaration *v);
    Statement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }

    DtorExpStatement *isDtorExpStatement() { return this; }
};

class CompileStatement : public Statement
{
public:
    Expression *exp;

    CompileStatement(Loc loc, Expression *exp);
    Statement *syntaxCopy();
    Statements *flatten(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class CompoundStatement : public Statement
{
public:
    Statements *statements;

    CompoundStatement(Loc loc, Statements *s);
    CompoundStatement(Loc loc, Statement *s1);
    CompoundStatement(Loc loc, Statement *s1, Statement *s2);
    static CompoundStatement *create(Loc loc, Statement *s1, Statement *s2);
    Statement *syntaxCopy();
    Statements *flatten(Scope *sc);
    ReturnStatement *isReturnStatement();
    Statement *last();

    CompoundStatement *isCompoundStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class CompoundDeclarationStatement : public CompoundStatement
{
public:
    CompoundDeclarationStatement(Loc loc, Statements *s);
    Statement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

/* The purpose of this is so that continue will go to the next
 * of the statements, and break will go to the end of the statements.
 */
class UnrolledLoopStatement : public Statement
{
public:
    Statements *statements;

    UnrolledLoopStatement(Loc loc, Statements *statements);
    Statement *syntaxCopy();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class ScopeStatement : public Statement
{
public:
    Statement *statement;
    Loc endloc;                 // location of closing curly bracket

    ScopeStatement(Loc loc, Statement *s, Loc endloc);
    Statement *syntaxCopy();
    ScopeStatement *isScopeStatement() { return this; }
    ReturnStatement *isReturnStatement();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class ForwardingStatement : public Statement
{
    ForwardingScopeDsymbol *sym;
    Statement *statement;

    Statement *syntaxCopy();
    Statement *getRelatedLabeled();
    bool hasBreak();
    bool hasContinue();
    Statement *scopeCode(Scope *sc, Statement **sentry, Statement **sexception, Statement **sfinally);
    Statement *last();
    Statements *flatten(Scope *sc);
    ForwardingStatement *isForwardingStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class WhileStatement : public Statement
{
public:
    Expression *condition;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    WhileStatement(Loc loc, Expression *c, Statement *b, Loc endloc);
    Statement *syntaxCopy();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class DoStatement : public Statement
{
public:
    Statement *_body;
    Expression *condition;
    Loc endloc;                 // location of ';' after while

    DoStatement(Loc loc, Statement *b, Expression *c, Loc endloc);
    Statement *syntaxCopy();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class ForStatement : public Statement
{
public:
    Statement *_init;
    Expression *condition;
    Expression *increment;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    // When wrapped in try/finally clauses, this points to the outermost one,
    // which may have an associated label. Internal break/continue statements
    // treat that label as referring to this loop.
    Statement *relatedLabeled;

    ForStatement(Loc loc, Statement *init, Expression *condition, Expression *increment, Statement *body, Loc endloc);
    Statement *syntaxCopy();
    Statement *scopeCode(Scope *sc, Statement **sentry, Statement **sexit, Statement **sfinally);
    Statement *getRelatedLabeled() { return relatedLabeled ? relatedLabeled : this; }
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class ForeachStatement : public Statement
{
public:
    TOK op;                     // TOKforeach or TOKforeach_reverse
    Parameters *parameters;     // array of Parameter*'s
    Expression *aggr;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    VarDeclaration *key;
    VarDeclaration *value;

    FuncDeclaration *func;      // function we're lexically in

    Statements *cases;          // put breaks, continues, gotos and returns here
    ScopeStatements *gotos;     // forward referenced goto's go here

    ForeachStatement(Loc loc, TOK op, Parameters *parameters, Expression *aggr, Statement *body, Loc endloc);
    Statement *syntaxCopy();
    bool checkForArgTypes();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class ForeachRangeStatement : public Statement
{
public:
    TOK op;                     // TOKforeach or TOKforeach_reverse
    Parameter *prm;             // loop index variable
    Expression *lwr;
    Expression *upr;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    VarDeclaration *key;

    ForeachRangeStatement(Loc loc, TOK op, Parameter *prm,
        Expression *lwr, Expression *upr, Statement *body, Loc endloc);
    Statement *syntaxCopy();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class IfStatement : public Statement
{
public:
    Parameter *prm;
    Expression *condition;
    Statement *ifbody;
    Statement *elsebody;
    Loc endloc;                 // location of closing curly bracket

    VarDeclaration *match;      // for MatchExpression results

    IfStatement(Loc loc, Parameter *prm, Expression *condition, Statement *ifbody, Statement *elsebody, Loc endloc);
    Statement *syntaxCopy();
    IfStatement *isIfStatement() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class ConditionalStatement : public Statement
{
public:
    Condition *condition;
    Statement *ifbody;
    Statement *elsebody;

    ConditionalStatement(Loc loc, Condition *condition, Statement *ifbody, Statement *elsebody);
    Statement *syntaxCopy();
    Statements *flatten(Scope *sc);

    void accept(Visitor *v) { v->visit(this); }
};

class StaticForeachStatement : public Statement
{
public:
    StaticForeach *sfe;

    Statement *syntaxCopy();
    Statements *flatten(Scope *sc);

    void accept(Visitor *v) { v->visit(this); }
};

class PragmaStatement : public Statement
{
public:
    Identifier *ident;
    Expressions *args;          // array of Expression's
    Statement *_body;

    PragmaStatement(Loc loc, Identifier *ident, Expressions *args, Statement *body);
    Statement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class StaticAssertStatement : public Statement
{
public:
    StaticAssert *sa;

    StaticAssertStatement(StaticAssert *sa);
    Statement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class SwitchStatement : public Statement
{
public:
    Expression *condition;
    Statement *_body;
    bool isFinal;

    DefaultStatement *sdefault;
    TryFinallyStatement *tf;
    GotoCaseStatements gotoCases;  // array of unresolved GotoCaseStatement's
    CaseStatements *cases;         // array of CaseStatement's
    int hasNoDefault;           // !=0 if no default statement
    int hasVars;                // !=0 if has variable case values
    VarDeclaration *lastVar;

    SwitchStatement(Loc loc, Expression *c, Statement *b, bool isFinal);
    Statement *syntaxCopy();
    bool hasBreak();
    bool checkLabel();

    void accept(Visitor *v) { v->visit(this); }
};

class CaseStatement : public Statement
{
public:
    Expression *exp;
    Statement *statement;

    int index;          // which case it is (since we sort this)
    VarDeclaration *lastVar;

    CaseStatement(Loc loc, Expression *exp, Statement *s);
    Statement *syntaxCopy();
    int compare(RootObject *obj);
    CaseStatement *isCaseStatement() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};


class CaseRangeStatement : public Statement
{
public:
    Expression *first;
    Expression *last;
    Statement *statement;

    CaseRangeStatement(Loc loc, Expression *first, Expression *last, Statement *s);
    Statement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};


class DefaultStatement : public Statement
{
public:
    Statement *statement;
    VarDeclaration *lastVar;

    DefaultStatement(Loc loc, Statement *s);
    Statement *syntaxCopy();
    DefaultStatement *isDefaultStatement() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class GotoDefaultStatement : public Statement
{
public:
    SwitchStatement *sw;

    GotoDefaultStatement(Loc loc);
    Statement *syntaxCopy();
    GotoDefaultStatement *isGotoDefaultStatement() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class GotoCaseStatement : public Statement
{
public:
    Expression *exp;            // NULL, or which case to goto
    CaseStatement *cs;          // case statement it resolves to

    GotoCaseStatement(Loc loc, Expression *exp);
    Statement *syntaxCopy();
    GotoCaseStatement *isGotoCaseStatement() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class SwitchErrorStatement : public Statement
{
public:
    SwitchErrorStatement(Loc loc);

    void accept(Visitor *v) { v->visit(this); }
};

class ReturnStatement : public Statement
{
public:
    Expression *exp;
    size_t caseDim;

    ReturnStatement(Loc loc, Expression *exp);
    Statement *syntaxCopy();

    ReturnStatement *isReturnStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class BreakStatement : public Statement
{
public:
    Identifier *ident;

    BreakStatement(Loc loc, Identifier *ident);
    Statement *syntaxCopy();

    BreakStatement *isBreakStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class ContinueStatement : public Statement
{
public:
    Identifier *ident;

    ContinueStatement(Loc loc, Identifier *ident);
    Statement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class SynchronizedStatement : public Statement
{
public:
    Expression *exp;
    Statement *_body;

    SynchronizedStatement(Loc loc, Expression *exp, Statement *body);
    Statement *syntaxCopy();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class WithStatement : public Statement
{
public:
    Expression *exp;
    Statement *_body;
    VarDeclaration *wthis;
    Loc endloc;

    WithStatement(Loc loc, Expression *exp, Statement *body, Loc endloc);
    Statement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class TryCatchStatement : public Statement
{
public:
    Statement *_body;
    Catches *catches;

    TryCatchStatement(Loc loc, Statement *body, Catches *catches);
    Statement *syntaxCopy();
    bool hasBreak();

    void accept(Visitor *v) { v->visit(this); }
};

class Catch : public RootObject
{
public:
    Loc loc;
    Type *type;
    Identifier *ident;
    VarDeclaration *var;
    Statement *handler;

    // set if semantic processing errors
    bool errors;

    // was generated by the compiler,
    // wasn't present in source code
    bool internalCatch;

    Catch(Loc loc, Type *t, Identifier *id, Statement *handler);
    Catch *syntaxCopy();
};

class TryFinallyStatement : public Statement
{
public:
    Statement *_body;
    Statement *finalbody;

    TryFinallyStatement(Loc loc, Statement *body, Statement *finalbody);
    static TryFinallyStatement *create(Loc loc, Statement *body, Statement *finalbody);
    Statement *syntaxCopy();
    bool hasBreak();
    bool hasContinue();

    void accept(Visitor *v) { v->visit(this); }
};

class OnScopeStatement : public Statement
{
public:
    TOK tok;
    Statement *statement;

    OnScopeStatement(Loc loc, TOK tok, Statement *statement);
    Statement *syntaxCopy();
    Statement *scopeCode(Scope *sc, Statement **sentry, Statement **sexit, Statement **sfinally);

    void accept(Visitor *v) { v->visit(this); }
};

class ThrowStatement : public Statement
{
public:
    Expression *exp;
    // was generated by the compiler,
    // wasn't present in source code
    bool internalThrow;

    ThrowStatement(Loc loc, Expression *exp);
    Statement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class DebugStatement : public Statement
{
public:
    Statement *statement;

    DebugStatement(Loc loc, Statement *statement);
    Statement *syntaxCopy();
    Statements *flatten(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class GotoStatement : public Statement
{
public:
    Identifier *ident;
    LabelDsymbol *label;
    TryFinallyStatement *tf;
    OnScopeStatement *os;
    VarDeclaration *lastVar;

    GotoStatement(Loc loc, Identifier *ident);
    Statement *syntaxCopy();
    bool checkLabel();

    void accept(Visitor *v) { v->visit(this); }
};

class LabelStatement : public Statement
{
public:
    Identifier *ident;
    Statement *statement;
    TryFinallyStatement *tf;
    OnScopeStatement *os;
    VarDeclaration *lastVar;
    Statement *gotoTarget;      // interpret

    bool breaks;                // someone did a 'break ident'

    LabelStatement(Loc loc, Identifier *ident, Statement *statement);
    Statement *syntaxCopy();
    Statements *flatten(Scope *sc);
    Statement *scopeCode(Scope *sc, Statement **sentry, Statement **sexit, Statement **sfinally);

    LabelStatement *isLabelStatement() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class LabelDsymbol : public Dsymbol
{
public:
    LabelStatement *statement;

    LabelDsymbol(Identifier *ident);
    static LabelDsymbol *create(Identifier *ident);
    LabelDsymbol *isLabel();
    void accept(Visitor *v) { v->visit(this); }
};

Statement* asmSemantic(AsmStatement *s, Scope *sc);

class AsmStatement : public Statement
{
public:
    Token *tokens;

    AsmStatement(Loc loc, Token *tokens);
    Statement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class InlineAsmStatement : public AsmStatement
{
public:
    code *asmcode;
    unsigned asmalign;          // alignment of this statement
    unsigned regs;              // mask of registers modified (must match regm_t in back end)
    bool refparam;              // true if function parameter is referenced
    bool naked;                 // true if function is to be naked

    InlineAsmStatement(Loc loc, Token *tokens);
    Statement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

// A GCC asm statement - assembler instructions with D expression operands
class GccAsmStatement : public AsmStatement
{
public:
    StorageClass stc;           // attributes of the asm {} block
    Expression *insn;           // string expression that is the template for assembler code
    Expressions *args;          // input and output operands of the statement
    unsigned outputargs;        // of the operands in 'args', the number of output operands
    Identifiers *names;         // list of symbolic names for the operands
    Expressions *constraints;   // list of string constants specifying constraints on operands
    Expressions *clobbers;      // list of string constants specifying clobbers and scratch registers
    Identifiers *labels;        // list of goto labels
    GotoStatements *gotos;      // of the goto labels, the equivalent statements they represent

    GccAsmStatement(Loc loc, Token *tokens);
    Statement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

// a complete asm {} block
class CompoundAsmStatement : public CompoundStatement
{
public:
    StorageClass stc; // postfix attributes like nothrow/pure/@trusted

    CompoundAsmStatement(Loc loc, Statements *s, StorageClass stc);
    CompoundAsmStatement *syntaxCopy();
    Statements *flatten(Scope *sc);

    void accept(Visitor *v) { v->visit(this); }
};

class ImportStatement : public Statement
{
public:
    Dsymbols *imports;          // Array of Import's

    ImportStatement(Loc loc, Dsymbols *imports);
    Statement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};
