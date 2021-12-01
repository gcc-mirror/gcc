
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/statement.h
 */

#pragma once

#include "arraytypes.h"
#include "ast_node.h"
#include "dsymbol.h"
#include "visitor.h"
#include "tokens.h"

struct Scope;
class Expression;
class LabelDsymbol;
class Identifier;
class IfStatement;
class ExpStatement;
class DefaultStatement;
class VarDeclaration;
class Condition;
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

typedef unsigned char STMT;
enum
{
    STMTerror,
    STMTpeel,
    STMTexp, STMTdtorExp,
    STMTcompile,
    STMTcompound, STMTcompoundDeclaration, STMTcompoundAsm,
    STMTunrolledLoop,
    STMTscope,
    STMTforwarding,
    STMTwhile,
    STMTdo,
    STMTfor,
    STMTforeach,
    STMTforeachRange,
    STMTif,
    STMTconditional,
    STMTstaticForeach,
    STMTpragma,
    STMTstaticAssert,
    STMTswitch,
    STMTcase,
    STMTcaseRange,
    STMTdefault,
    STMTgotoDefault,
    STMTgotoCase,
    STMTswitchError,
    STMTreturn,
    STMTbreak,
    STMTcontinue,
    STMTsynchronized,
    STMTwith,
    STMTtryCatch,
    STMTtryFinally,
    STMTscopeGuard,
    STMTthrow,
    STMTdebug,
    STMTgoto,
    STMTlabel,
    STMTasm, STMTinlineAsm, STMTgccAsm,
    STMTimport
};

class Statement : public ASTNode
{
public:
    Loc loc;
    STMT stmt;

    virtual Statement *syntaxCopy();

    const char *toChars() const;

    void error(const char *format, ...);
    void warning(const char *format, ...);
    void deprecation(const char *format, ...);
    virtual Statement *getRelatedLabeled() { return this; }
    virtual bool hasBreak() const;
    virtual bool hasContinue() const;
    bool usesEH();
    bool comeFrom();
    bool hasCode();
    virtual Statement *last();

    virtual ReturnStatement *endsWithReturnStatement() { return NULL; }

    ErrorStatement       *isErrorStatement()       { return stmt == STMTerror       ? (ErrorStatement*)this       : NULL; }
    ScopeStatement       *isScopeStatement()       { return stmt == STMTscope       ? (ScopeStatement*)this       : NULL; }
    ExpStatement         *isExpStatement()         { return stmt == STMTexp         ? (ExpStatement*)this         : NULL; }
    CompoundStatement    *isCompoundStatement()    { return stmt == STMTcompound    ? (CompoundStatement*)this    : NULL; }
    ReturnStatement      *isReturnStatement()      { return stmt == STMTreturn      ? (ReturnStatement*)this      : NULL; }
    IfStatement          *isIfStatement()          { return stmt == STMTif          ? (IfStatement*)this          : NULL; }
    ConditionalStatement *isConditionalStatement() { return stmt == STMTconditional ? (ConditionalStatement*)this : NULL; }
    StaticForeachStatement *isStaticForeachStatement() { return stmt == STMTstaticForeach ? (StaticForeachStatement*)this : NULL; }
    CaseStatement        *isCaseStatement()        { return stmt == STMTcase        ? (CaseStatement*)this        : NULL; }
    DefaultStatement     *isDefaultStatement()     { return stmt == STMTdefault     ? (DefaultStatement*)this     : NULL; }
    LabelStatement       *isLabelStatement()       { return stmt == STMTlabel       ? (LabelStatement*)this       : NULL; }
    GotoDefaultStatement *isGotoDefaultStatement() { return stmt == STMTgotoDefault ? (GotoDefaultStatement*)this : NULL; }
    GotoCaseStatement    *isGotoCaseStatement()    { return stmt == STMTgotoCase    ? (GotoCaseStatement*)this    : NULL; }
    BreakStatement       *isBreakStatement()       { return stmt == STMTbreak       ? (BreakStatement*)this       : NULL; }
    DtorExpStatement     *isDtorExpStatement()     { return stmt == STMTdtorExp     ? (DtorExpStatement*)this     : NULL; }
    CompileStatement     *isCompileStatement()     { return stmt == STMTcompile     ? (CompileStatement*)this     : NULL; }
    ForwardingStatement  *isForwardingStatement()  { return stmt == STMTforwarding  ? (ForwardingStatement*)this  : NULL; }
    DoStatement          *isDoStatement()          { return stmt == STMTdo          ? (DoStatement*)this          : NULL; }
    ForStatement         *isForStatement()         { return stmt == STMTfor         ? (ForStatement*)this         : NULL; }
    ForeachStatement     *isForeachStatement()     { return stmt == STMTforeach     ? (ForeachStatement*)this     : NULL; }
    SwitchStatement      *isSwitchStatement()      { return stmt == STMTswitch      ? (SwitchStatement*)this      : NULL; }
    ContinueStatement    *isContinueStatement()    { return stmt == STMTcontinue    ? (ContinueStatement*)this    : NULL; }
    WithStatement        *isWithStatement()        { return stmt == STMTwith        ? (WithStatement*)this        : NULL; }
    TryCatchStatement    *isTryCatchStatement()    { return stmt == STMTtryCatch    ? (TryCatchStatement*)this    : NULL; }
    ThrowStatement       *isThrowStatement()       { return stmt == STMTthrow       ? (ThrowStatement*)this       : NULL; }
    DebugStatement       *isDebugStatement()       { return stmt == STMTdebug       ? (DebugStatement*)this       : NULL; }
    TryFinallyStatement  *isTryFinallyStatement()  { return stmt == STMTtryFinally  ? (TryFinallyStatement*)this  : NULL; }
    ScopeGuardStatement  *isScopeGuardStatement()  { return stmt == STMTscopeGuard  ? (ScopeGuardStatement*)this  : NULL; }
    SwitchErrorStatement  *isSwitchErrorStatement()  { return stmt == STMTswitchError  ? (SwitchErrorStatement*)this  : NULL; }
    UnrolledLoopStatement *isUnrolledLoopStatement() { return stmt == STMTunrolledLoop ? (UnrolledLoopStatement*)this : NULL; }
    ForeachRangeStatement *isForeachRangeStatement() { return stmt == STMTforeachRange ? (ForeachRangeStatement*)this : NULL; }
    CompoundDeclarationStatement *isCompoundDeclarationStatement() { return stmt == STMTcompoundDeclaration ? (CompoundDeclarationStatement*)this : NULL; }

    void accept(Visitor *v) { v->visit(this); }
};

/** Any Statement that fails semantic() or has a component that is an ErrorExp or
 * a TypeError should return an ErrorStatement from semantic().
 */
class ErrorStatement : public Statement
{
public:
    ErrorStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class PeelStatement : public Statement
{
public:
    Statement *s;

    void accept(Visitor *v) { v->visit(this); }
};

class ExpStatement : public Statement
{
public:
    Expression *exp;

    static ExpStatement *create(Loc loc, Expression *exp);
    ExpStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class DtorExpStatement : public ExpStatement
{
public:
    /* Wraps an expression that is the destruction of 'var'
     */

    VarDeclaration *var;

    DtorExpStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class CompileStatement : public Statement
{
public:
    Expressions *exps;

    CompileStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class CompoundStatement : public Statement
{
public:
    Statements *statements;

    static CompoundStatement *create(Loc loc, Statement *s1, Statement *s2);
    CompoundStatement *syntaxCopy();
    ReturnStatement *endsWithReturnStatement();
    Statement *last();

    void accept(Visitor *v) { v->visit(this); }
};

class CompoundDeclarationStatement : public CompoundStatement
{
public:
    CompoundDeclarationStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

/* The purpose of this is so that continue will go to the next
 * of the statements, and break will go to the end of the statements.
 */
class UnrolledLoopStatement : public Statement
{
public:
    Statements *statements;

    UnrolledLoopStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

    void accept(Visitor *v) { v->visit(this); }
};

class ScopeStatement : public Statement
{
public:
    Statement *statement;
    Loc endloc;                 // location of closing curly bracket

    ScopeStatement *syntaxCopy();
    ReturnStatement *endsWithReturnStatement();
    bool hasBreak() const;
    bool hasContinue() const;

    void accept(Visitor *v) { v->visit(this); }
};

class ForwardingStatement : public Statement
{
public:
    ForwardingScopeDsymbol *sym;
    Statement *statement;

    ForwardingStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class WhileStatement : public Statement
{
public:
    Parameter *param;
    Expression *condition;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    WhileStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

    void accept(Visitor *v) { v->visit(this); }
};

class DoStatement : public Statement
{
public:
    Statement *_body;
    Expression *condition;
    Loc endloc;                 // location of ';' after while

    DoStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

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

    ForStatement *syntaxCopy();
    Statement *getRelatedLabeled() { return relatedLabeled ? relatedLabeled : this; }
    bool hasBreak() const;
    bool hasContinue() const;

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

    ForeachStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

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

    ForeachRangeStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

    void accept(Visitor *v) { v->visit(this); }
};

class IfStatement : public Statement
{
public:
    Parameter *prm;
    Expression *condition;
    Statement *ifbody;
    Statement *elsebody;
    VarDeclaration *match;      // for MatchExpression results
    Loc endloc;                 // location of closing curly bracket

    IfStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class ConditionalStatement : public Statement
{
public:
    Condition *condition;
    Statement *ifbody;
    Statement *elsebody;

    ConditionalStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class StaticForeachStatement : public Statement
{
public:
    StaticForeach *sfe;

    StaticForeachStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class PragmaStatement : public Statement
{
public:
    Identifier *ident;
    Expressions *args;          // array of Expression's
    Statement *_body;

    PragmaStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class StaticAssertStatement : public Statement
{
public:
    StaticAssert *sa;

    StaticAssertStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class SwitchStatement : public Statement
{
public:
    Expression *condition;
    Statement *_body;
    bool isFinal;

    DefaultStatement *sdefault;
    Statement *tryBody;            // set to TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement *tf;
    GotoCaseStatements gotoCases;  // array of unresolved GotoCaseStatement's
    CaseStatements *cases;         // array of CaseStatement's
    int hasNoDefault;           // !=0 if no default statement
    int hasVars;                // !=0 if has variable case values
    VarDeclaration *lastVar;

    SwitchStatement *syntaxCopy();
    bool hasBreak() const;

    void accept(Visitor *v) { v->visit(this); }
};

class CaseStatement : public Statement
{
public:
    Expression *exp;
    Statement *statement;

    int index;          // which case it is (since we sort this)
    VarDeclaration *lastVar;
    void* extra;            // for use by Statement_toIR()

    CaseStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};


class CaseRangeStatement : public Statement
{
public:
    Expression *first;
    Expression *last;
    Statement *statement;

    CaseRangeStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};


class DefaultStatement : public Statement
{
public:
    Statement *statement;
    VarDeclaration *lastVar;

    DefaultStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class GotoDefaultStatement : public Statement
{
public:
    SwitchStatement *sw;

    GotoDefaultStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class GotoCaseStatement : public Statement
{
public:
    Expression *exp;            // NULL, or which case to goto
    CaseStatement *cs;          // case statement it resolves to

    GotoCaseStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class SwitchErrorStatement : public Statement
{
public:
    Expression *exp;

    void accept(Visitor *v) { v->visit(this); }
};

class ReturnStatement : public Statement
{
public:
    Expression *exp;
    size_t caseDim;

    ReturnStatement *syntaxCopy();

    ReturnStatement *endsWithReturnStatement() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class BreakStatement : public Statement
{
public:
    Identifier *ident;

    BreakStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class ContinueStatement : public Statement
{
public:
    Identifier *ident;

    ContinueStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class SynchronizedStatement : public Statement
{
public:
    Expression *exp;
    Statement *_body;

    SynchronizedStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

    void accept(Visitor *v) { v->visit(this); }
};

class WithStatement : public Statement
{
public:
    Expression *exp;
    Statement *_body;
    VarDeclaration *wthis;
    Loc endloc;

    WithStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class TryCatchStatement : public Statement
{
public:
    Statement *_body;
    Catches *catches;

    Statement *tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion

    TryCatchStatement *syntaxCopy();
    bool hasBreak() const;

    void accept(Visitor *v) { v->visit(this); }
};

class Catch : public RootObject
{
public:
    Loc loc;
    Type *type;
    Identifier *ident;
    Statement *handler;

    VarDeclaration *var;
    // set if semantic processing errors
    bool errors;

    // was generated by the compiler,
    // wasn't present in source code
    bool internalCatch;

    Catch *syntaxCopy();
};

class TryFinallyStatement : public Statement
{
public:
    Statement *_body;
    Statement *finalbody;

    Statement *tryBody;   // set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion
    bool bodyFallsThru;   // true if _body falls through to finally

    static TryFinallyStatement *create(Loc loc, Statement *body, Statement *finalbody);
    TryFinallyStatement *syntaxCopy();
    bool hasBreak() const;
    bool hasContinue() const;

    void accept(Visitor *v) { v->visit(this); }
};

class ScopeGuardStatement : public Statement
{
public:
    TOK tok;
    Statement *statement;

    ScopeGuardStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class ThrowStatement : public Statement
{
public:
    Expression *exp;
    // was generated by the compiler,
    // wasn't present in source code
    bool internalThrow;

    ThrowStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class DebugStatement : public Statement
{
public:
    Statement *statement;

    DebugStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class GotoStatement : public Statement
{
public:
    Identifier *ident;
    LabelDsymbol *label;
    Statement *tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement *tf;
    ScopeGuardStatement *os;
    VarDeclaration *lastVar;

    GotoStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class LabelStatement : public Statement
{
public:
    Identifier *ident;
    Statement *statement;
    Statement *tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement *tf;
    ScopeGuardStatement *os;
    VarDeclaration *lastVar;
    Statement *gotoTarget;      // interpret
    void* extra;                // used by Statement_toIR()
    bool breaks;                // someone did a 'break ident'

    LabelStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class LabelDsymbol : public Dsymbol
{
public:
    LabelStatement *statement;

    bool deleted;           // set if rewritten to return in foreach delegate
    bool iasm;              // set if used by inline assembler

    static LabelDsymbol *create(Identifier *ident);
    LabelDsymbol *isLabel();
    void accept(Visitor *v) { v->visit(this); }
};

Statement* asmSemantic(AsmStatement *s, Scope *sc);

class AsmStatement : public Statement
{
public:
    Token *tokens;

    AsmStatement *syntaxCopy();
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

    InlineAsmStatement *syntaxCopy();
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

    GccAsmStatement *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

// a complete asm {} block
class CompoundAsmStatement : public CompoundStatement
{
public:
    StorageClass stc; // postfix attributes like nothrow/pure/@trusted

    CompoundAsmStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

class ImportStatement : public Statement
{
public:
    Dsymbols *imports;          // Array of Import's

    ImportStatement *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};
