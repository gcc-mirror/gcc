
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
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
enum BE : int32_t
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
    STMTmixin,
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

    DYNCAST dyncast() const override final { return DYNCAST_STATEMENT; }

    virtual Statement *syntaxCopy();

    virtual Statement *getRelatedLabeled() { return this; }
    virtual bool hasBreak() const;
    virtual bool hasContinue() const;
    bool hasCode();
    virtual Statement *last();

    virtual ReturnStatement *endsWithReturnStatement() { return nullptr; }

    ErrorStatement       *isErrorStatement()       { return stmt == STMTerror       ? (ErrorStatement*)this       : nullptr; }
    ScopeStatement       *isScopeStatement()       { return stmt == STMTscope       ? (ScopeStatement*)this       : nullptr; }
    ExpStatement         *isExpStatement()         { return stmt == STMTexp         ? (ExpStatement*)this         : nullptr; }
    CompoundStatement    *isCompoundStatement()    { return stmt == STMTcompound    ? (CompoundStatement*)this    : nullptr; }
    ReturnStatement      *isReturnStatement()      { return stmt == STMTreturn      ? (ReturnStatement*)this      : nullptr; }
    IfStatement          *isIfStatement()          { return stmt == STMTif          ? (IfStatement*)this          : nullptr; }
    ConditionalStatement *isConditionalStatement() { return stmt == STMTconditional ? (ConditionalStatement*)this : nullptr; }
    StaticForeachStatement *isStaticForeachStatement() { return stmt == STMTstaticForeach ? (StaticForeachStatement*)this : nullptr; }
    CaseStatement        *isCaseStatement()        { return stmt == STMTcase        ? (CaseStatement*)this        : nullptr; }
    DefaultStatement     *isDefaultStatement()     { return stmt == STMTdefault     ? (DefaultStatement*)this     : nullptr; }
    LabelStatement       *isLabelStatement()       { return stmt == STMTlabel       ? (LabelStatement*)this       : nullptr; }
    GotoDefaultStatement *isGotoDefaultStatement() { return stmt == STMTgotoDefault ? (GotoDefaultStatement*)this : nullptr; }
    GotoCaseStatement    *isGotoCaseStatement()    { return stmt == STMTgotoCase    ? (GotoCaseStatement*)this    : nullptr; }
    BreakStatement       *isBreakStatement()       { return stmt == STMTbreak       ? (BreakStatement*)this       : nullptr; }
    DtorExpStatement     *isDtorExpStatement()     { return stmt == STMTdtorExp     ? (DtorExpStatement*)this     : nullptr; }
    MixinStatement       *isMixinStatement()       { return stmt == STMTmixin       ? (MixinStatement*)this       : nullptr; }
    ForwardingStatement  *isForwardingStatement()  { return stmt == STMTforwarding  ? (ForwardingStatement*)this  : nullptr; }
    DoStatement          *isDoStatement()          { return stmt == STMTdo          ? (DoStatement*)this          : nullptr; }
    ForStatement         *isForStatement()         { return stmt == STMTfor         ? (ForStatement*)this         : nullptr; }
    ForeachStatement     *isForeachStatement()     { return stmt == STMTforeach     ? (ForeachStatement*)this     : nullptr; }
    SwitchStatement      *isSwitchStatement()      { return stmt == STMTswitch      ? (SwitchStatement*)this      : nullptr; }
    ContinueStatement    *isContinueStatement()    { return stmt == STMTcontinue    ? (ContinueStatement*)this    : nullptr; }
    WithStatement        *isWithStatement()        { return stmt == STMTwith        ? (WithStatement*)this        : nullptr; }
    TryCatchStatement    *isTryCatchStatement()    { return stmt == STMTtryCatch    ? (TryCatchStatement*)this    : nullptr; }
    ThrowStatement       *isThrowStatement()       { return stmt == STMTthrow       ? (ThrowStatement*)this       : nullptr; }
    DebugStatement       *isDebugStatement()       { return stmt == STMTdebug       ? (DebugStatement*)this       : nullptr; }
    TryFinallyStatement  *isTryFinallyStatement()  { return stmt == STMTtryFinally  ? (TryFinallyStatement*)this  : nullptr; }
    ScopeGuardStatement  *isScopeGuardStatement()  { return stmt == STMTscopeGuard  ? (ScopeGuardStatement*)this  : nullptr; }
    SwitchErrorStatement  *isSwitchErrorStatement()  { return stmt == STMTswitchError  ? (SwitchErrorStatement*)this  : nullptr; }
    UnrolledLoopStatement *isUnrolledLoopStatement() { return stmt == STMTunrolledLoop ? (UnrolledLoopStatement*)this : nullptr; }
    ForeachRangeStatement *isForeachRangeStatement() { return stmt == STMTforeachRange ? (ForeachRangeStatement*)this : nullptr; }
    CompoundDeclarationStatement *isCompoundDeclarationStatement() { return stmt == STMTcompoundDeclaration ? (CompoundDeclarationStatement*)this : nullptr; }

    void accept(Visitor *v) override { v->visit(this); }
};

/** Any Statement that fails semantic() or has a component that is an ErrorExp or
 * a TypeError should return an ErrorStatement from semantic().
 */
class ErrorStatement final : public Statement
{
public:
    ErrorStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class PeelStatement final : public Statement
{
public:
    Statement *s;

    void accept(Visitor *v) override { v->visit(this); }
};

class ExpStatement : public Statement
{
public:
    Expression *exp;

    static ExpStatement *create(const Loc &loc, Expression *exp);
    ExpStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class DtorExpStatement final : public ExpStatement
{
public:
    /* Wraps an expression that is the destruction of 'var'
     */

    VarDeclaration *var;

    DtorExpStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class MixinStatement final : public Statement
{
public:
    Expressions *exps;

    MixinStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class CompoundStatement : public Statement
{
public:
    Statements *statements;

    static CompoundStatement *create(const Loc &loc, Statement *s1, Statement *s2);
    CompoundStatement *syntaxCopy() override;
    ReturnStatement *endsWithReturnStatement() override final;
    Statement *last() override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class CompoundDeclarationStatement final : public CompoundStatement
{
public:
    CompoundDeclarationStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/* The purpose of this is so that continue will go to the next
 * of the statements, and break will go to the end of the statements.
 */
class UnrolledLoopStatement final : public Statement
{
public:
    Statements *statements;

    UnrolledLoopStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ScopeStatement final : public Statement
{
public:
    Statement *statement;
    Loc endloc;                 // location of closing curly bracket

    ScopeStatement *syntaxCopy() override;
    ReturnStatement *endsWithReturnStatement() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ForwardingStatement final : public Statement
{
public:
    ForwardingScopeDsymbol *sym;
    Statement *statement;

    ForwardingStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class WhileStatement final : public Statement
{
public:
    Parameter *param;
    Expression *condition;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    WhileStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class DoStatement final : public Statement
{
public:
    Statement *_body;
    Expression *condition;
    Loc endloc;                 // location of ';' after while

    DoStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ForStatement final : public Statement
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

    ForStatement *syntaxCopy() override;
    Statement *getRelatedLabeled() override { return relatedLabeled ? relatedLabeled : this; }
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ForeachStatement final : public Statement
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

    ForeachStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ForeachRangeStatement final : public Statement
{
public:
    TOK op;                     // TOKforeach or TOKforeach_reverse
    Parameter *param;             // loop index variable
    Expression *lwr;
    Expression *upr;
    Statement *_body;
    Loc endloc;                 // location of closing curly bracket

    VarDeclaration *key;

    ForeachRangeStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class IfStatement final : public Statement
{
public:
    Parameter *param;
    Expression *condition;
    Statement *ifbody;
    Statement *elsebody;
    VarDeclaration *match;      // for MatchExpression results
    Loc endloc;                 // location of closing curly bracket

    IfStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
    bool isIfCtfeBlock();
};

class ConditionalStatement final : public Statement
{
public:
    Condition *condition;
    Statement *ifbody;
    Statement *elsebody;

    ConditionalStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class StaticForeachStatement final : public Statement
{
public:
    StaticForeach *sfe;

    StaticForeachStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class PragmaStatement final : public Statement
{
public:
    Identifier *ident;
    Expressions *args;          // array of Expression's
    Statement *_body;

    PragmaStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class StaticAssertStatement final : public Statement
{
public:
    StaticAssert *sa;

    StaticAssertStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class SwitchStatement final : public Statement
{
public:
    Parameter *param;
    Expression *condition;
    Statement *_body;
    d_bool isFinal;
    Loc endloc;

    d_bool hasDefault;             // true if default statement
    d_bool hasVars;                // true if has variable case values
    DefaultStatement *sdefault;
    Statement *tryBody;            // set to TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement *tf;
    GotoCaseStatements gotoCases;  // array of unresolved GotoCaseStatement's
    CaseStatements *cases;         // array of CaseStatement's
    VarDeclaration *lastVar;

    SwitchStatement *syntaxCopy() override;
    bool hasBreak() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class CaseStatement final : public Statement
{
public:
    Expression *exp;
    Statement *statement;

    int index;          // which case it is (since we sort this)
    VarDeclaration *lastVar;
    void* extra;            // for use by Statement_toIR()

    CaseStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};


class CaseRangeStatement final : public Statement
{
public:
    Expression *first;
    Expression *last;
    Statement *statement;

    CaseRangeStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};


class DefaultStatement final : public Statement
{
public:
    Statement *statement;
    VarDeclaration *lastVar;

    DefaultStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class GotoDefaultStatement final : public Statement
{
public:
    SwitchStatement *sw;

    GotoDefaultStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class GotoCaseStatement final : public Statement
{
public:
    Expression *exp;            // NULL, or which case to goto
    CaseStatement *cs;          // case statement it resolves to

    GotoCaseStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class SwitchErrorStatement final : public Statement
{
public:
    Expression *exp;

    void accept(Visitor *v) override { v->visit(this); }
};

class ReturnStatement final : public Statement
{
public:
    Expression *exp;
    size_t caseDim;

    ReturnStatement *syntaxCopy() override;

    ReturnStatement *endsWithReturnStatement() override { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};

class BreakStatement final : public Statement
{
public:
    Identifier *ident;

    BreakStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ContinueStatement final : public Statement
{
public:
    Identifier *ident;

    ContinueStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class SynchronizedStatement final : public Statement
{
public:
    Expression *exp;
    Statement *_body;

    SynchronizedStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class WithStatement final : public Statement
{
public:
    Expression *exp;
    Statement *_body;
    VarDeclaration *wthis;
    Loc endloc;

    WithStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TryCatchStatement final : public Statement
{
public:
    Statement *_body;
    Catches *catches;

    Statement *tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion

    TryCatchStatement *syntaxCopy() override;
    bool hasBreak() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class Catch final : public RootObject
{
public:
    Loc loc;
    Type *type;
    Identifier *ident;
    Statement *handler;

    VarDeclaration *var;
    // set if semantic processing errors
    d_bool errors;

    // was generated by the compiler,
    // wasn't present in source code
    d_bool internalCatch;

    Catch *syntaxCopy();
};

class TryFinallyStatement final : public Statement
{
public:
    Statement *_body;
    Statement *finalbody;

    Statement *tryBody;   // set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion
    d_bool bodyFallsThru;   // true if _body falls through to finally

    static TryFinallyStatement *create(const Loc &loc, Statement *body, Statement *finalbody);
    TryFinallyStatement *syntaxCopy() override;
    bool hasBreak() const override;
    bool hasContinue() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ScopeGuardStatement final : public Statement
{
public:
    TOK tok;
    Statement *statement;

    ScopeGuardStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ThrowStatement final : public Statement
{
public:
    Expression *exp;
    // was generated by the compiler,
    // wasn't present in source code
    d_bool internalThrow;

    ThrowStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class DebugStatement final : public Statement
{
public:
    Statement *statement;

    DebugStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class GotoStatement final : public Statement
{
public:
    Identifier *ident;
    LabelDsymbol *label;
    Statement *tryBody;   /// set to enclosing TryCatchStatement or TryFinallyStatement if in _body portion
    TryFinallyStatement *tf;
    ScopeGuardStatement *os;
    VarDeclaration *lastVar;
    d_bool inCtfeBlock;
    GotoStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class LabelStatement final : public Statement
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
    d_bool breaks;                // someone did a 'break ident'
    d_bool inCtfeBlock;
    LabelStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class LabelDsymbol final : public Dsymbol
{
public:
    LabelStatement *statement;

    d_bool deleted;           // set if rewritten to return in foreach delegate
    d_bool iasm;              // set if used by inline assembler
    d_bool duplicated;        // set if multiply defined, to avoid duplicate error messages

    static LabelDsymbol *create(Identifier *ident);
    LabelDsymbol *isLabel() override;
    void accept(Visitor *v) override { v->visit(this); }
};

namespace dmd
{
    // in statementsem.d
    Statement* statementSemantic(Statement *s, Scope *sc);
    // in iasm.d
    Statement* asmSemantic(AsmStatement *s, Scope *sc);
    // in iasmgcc.d
    Statement *gccAsmSemantic(GccAsmStatement *s, Scope *sc);
}

class AsmStatement : public Statement
{
public:
    Token *tokens;
    d_bool caseSensitive;  // for register names

    AsmStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class InlineAsmStatement final : public AsmStatement
{
public:
    void *asmcode;
    unsigned asmalign;          // alignment of this statement
    unsigned regs;              // mask of registers modified (must match regm_t in back end)
    d_bool refparam;              // true if function parameter is referenced
    d_bool naked;                 // true if function is to be naked

    InlineAsmStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

// A GCC asm statement - assembler instructions with D expression operands
class GccAsmStatement final : public AsmStatement
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

    GccAsmStatement *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

// a complete asm {} block
class CompoundAsmStatement final : public CompoundStatement
{
public:
    StorageClass stc; // postfix attributes like nothrow/pure/@trusted

    CompoundAsmStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ImportStatement final : public Statement
{
public:
    Dsymbols *imports;          // Array of Import's

    ImportStatement *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};
