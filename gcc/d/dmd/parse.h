
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/parse.h
 */

#pragma once

#include "arraytypes.h"
#include "lexer.h"
#include "enum.h"

class Type;
class TypeQualified;
class Expression;
class Declaration;
class Statement;
class Import;
class Initializer;
class FuncDeclaration;
class CtorDeclaration;
class PostBlitDeclaration;
class DtorDeclaration;
class StaticCtorDeclaration;
class StaticDtorDeclaration;
class SharedStaticCtorDeclaration;
class SharedStaticDtorDeclaration;
class ConditionalDeclaration;
class InvariantDeclaration;
class UnitTestDeclaration;
class NewDeclaration;
class DeleteDeclaration;
class Condition;
class Module;
struct ModuleDeclaration;
class TemplateDeclaration;
class TemplateInstance;
class StaticAssert;
struct PrefixAttributes;

/************************************
 * These control how parseStatement() works.
 */

enum ParseStatementFlags
{
    PSsemi = 1,         // empty ';' statements are allowed, but deprecated
    PSscope = 2,        // start a new scope
    PScurly = 4,        // { } statement is required
    PScurlyscope = 8,   // { } starts a new scope
    PSsemi_ok = 0x10    // empty ';' are really ok
};


class Parser : public Lexer
{
public:
    Module *mod;
    ModuleDeclaration *md;
    LINK linkage;
    CPPMANGLE cppmangle;
    Loc endloc;                 // set to location of last right curly
    int inBrackets;             // inside [] of array index or slice
    Loc lookingForElse;         // location of lonely if looking for an else

    Parser(Loc loc, Module *module, const utf8_t *base, size_t length, bool doDocComment);
    Parser(Module *module, const utf8_t *base, size_t length, bool doDocComment);

    Dsymbols *parseModule();
    Dsymbols *parseDeclDefs(int once, Dsymbol **pLastDecl = NULL, PrefixAttributes *pAttrs = NULL);
    Dsymbols *parseAutoDeclarations(StorageClass storageClass, const utf8_t *comment);
    Dsymbols *parseBlock(Dsymbol **pLastDecl, PrefixAttributes *pAttrs = NULL);
    StorageClass appendStorageClass(StorageClass storageClass, StorageClass stc, bool deprec = false);
    StorageClass parseAttribute(Expressions **pexps);
    StorageClass parsePostfix(StorageClass storageClass, Expressions **pudas);
    StorageClass parseTypeCtor();
    Expression *parseConstraint();
    TemplateDeclaration *parseTemplateDeclaration(bool ismixin = false);
    TemplateParameters *parseTemplateParameterList(int flag = 0);
    Dsymbol *parseMixin();
    Objects *parseTemplateArguments();
    RootObject *parseTypeOrAssignExp(TOK endtoken = TOKreserved);
    Objects *parseTemplateArgumentList();
    Objects *parseTemplateSingleArgument();
    StaticAssert *parseStaticAssert();
    TypeQualified *parseTypeof();
    Type *parseVector();
    LINK parseLinkage(Identifiers **, CPPMANGLE *, bool *);
    Identifiers *parseQualifiedIdentifier(const char *entity);
    Condition *parseDebugCondition();
    Condition *parseVersionCondition();
    Condition *parseStaticIfCondition();
    Dsymbol *parseCtor(PrefixAttributes *pAttrs);
    Dsymbol *parseDtor(PrefixAttributes *pAttrs);
    Dsymbol *parseStaticCtor(PrefixAttributes *pAttrs);
    Dsymbol *parseStaticDtor(PrefixAttributes *pAttrs);
    Dsymbol *parseSharedStaticCtor(PrefixAttributes *pAttrs);
    Dsymbol *parseSharedStaticDtor(PrefixAttributes *pAttrs);
    Dsymbol *parseInvariant(PrefixAttributes *pAttrs);
    Dsymbol *parseUnitTest(PrefixAttributes *pAttrs);
    Dsymbol *parseNew(PrefixAttributes *pAttrs);
    Dsymbol *parseDelete(PrefixAttributes *pAttrs);
    Parameters *parseParameters(VarArg *pvarargs, TemplateParameters **tpl = NULL);
    EnumDeclaration *parseEnum();
    Dsymbol *parseAggregate();
    BaseClasses *parseBaseClasses();
    Dsymbols *parseImport();
    Type *parseType(Identifier **pident = NULL, TemplateParameters **ptpl = NULL);
    Type *parseBasicType(bool dontLookDotIdents = false);
    Type *parseBasicTypeStartingAt(TypeQualified *tid, bool dontLookDotIdents);
    Type *parseBasicType2(Type *t);
    Type *parseDeclarator(Type *t, int *alt, Identifier **pident,
        TemplateParameters **tpl = NULL, StorageClass storage_class = 0, int *pdisable = NULL, Expressions **pudas = NULL);
    void parseStorageClasses(StorageClass &storage_class, LINK &link, bool &setAlignment, Expression *&ealign, Expressions *&udas);
    Dsymbols *parseDeclarations(bool autodecl, PrefixAttributes *pAttrs, const utf8_t *comment);
    Dsymbol *parseFunctionLiteral();
    FuncDeclaration *parseContracts(FuncDeclaration *f);
    void checkDanglingElse(Loc elseloc);
    void checkCstyleTypeSyntax(Loc loc, Type *t, int alt, Identifier *ident);
    Statement *parseForeach(Loc loc, bool *isRange, bool isDecl);
    Dsymbol *parseForeachStaticDecl(Loc loc, Dsymbol **pLastDecl);
    Statement *parseForeachStatic(Loc loc);
    /** endPtr used for documented unittests */
    Statement *parseStatement(int flags, const utf8_t** endPtr = NULL, Loc *pEndloc = NULL);
    Initializer *parseInitializer();
    Expression *parseDefaultInitExp();
    void check(Loc loc, TOK value);
    void check(TOK value);
    void check(TOK value, const char *string);
    void checkParens(TOK value, Expression *e);
    bool isDeclaration(Token *t, int needId, TOK endtok, Token **pt);
    bool isBasicType(Token **pt);
    bool isDeclarator(Token **pt, int *haveId, int *haveTpl, TOK endtok, bool allowAltSyntax = true);
    bool isParameters(Token **pt);
    bool isExpression(Token **pt);
    bool skipParens(Token *t, Token **pt);
    bool skipParensIf(Token *t, Token **pt);
    bool skipAttributes(Token *t, Token **pt);

    Expression *parseExpression();
    Expression *parsePrimaryExp();
    Expression *parseUnaryExp();
    Expression *parsePostExp(Expression *e);
    Expression *parseMulExp();
    Expression *parseAddExp();
    Expression *parseShiftExp();
    Expression *parseCmpExp();
    Expression *parseAndExp();
    Expression *parseXorExp();
    Expression *parseOrExp();
    Expression *parseAndAndExp();
    Expression *parseOrOrExp();
    Expression *parseCondExp();
    Expression *parseAssignExp();

    Expressions *parseArguments();

    Expression *parseNewExp(Expression *thisexp);

    void addComment(Dsymbol *s, const utf8_t *blockComment);
};

// Operator precedence - greater values are higher precedence

enum PREC
{
    PREC_zero,
    PREC_expr,
    PREC_assign,
    PREC_cond,
    PREC_oror,
    PREC_andand,
    PREC_or,
    PREC_xor,
    PREC_and,
    PREC_equal,
    PREC_rel,
    PREC_shift,
    PREC_add,
    PREC_mul,
    PREC_pow,
    PREC_unary,
    PREC_primary
};

extern PREC precedence[TOKMAX];

void initPrecedence();
