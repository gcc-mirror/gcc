
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/template.h
 */

#pragma once

#include "arraytypes.h"
#include "dsymbol.h"
#include "expression.h"

class Identifier;
class TemplateInstance;
class TemplateParameter;
class TemplateTypeParameter;
class TemplateThisParameter;
class TemplateValueParameter;
class TemplateAliasParameter;
class TemplateTupleParameter;
class Type;
class TypeQualified;
struct Scope;
class Expression;
class FuncDeclaration;
class Parameter;

class Tuple final : public RootObject
{
public:
    Objects objects;

    // kludge for template.isType()
    DYNCAST dyncast() const override { return DYNCAST_TUPLE; }

    const char *toChars() const override;
};

struct TemplatePrevious
{
    TemplatePrevious *prev;
    Scope *sc;
    Objects *dedargs;
};


class TemplateDeclaration final : public ScopeDsymbol
{
public:
    TemplateParameters *parameters;     // array of TemplateParameter's

    TemplateParameters *origParameters; // originals for Ddoc
    Expression *constraint;

    // Hash table to look up TemplateInstance's of this TemplateDeclaration
    void *instances;

    TemplateDeclaration *overnext;      // next overloaded TemplateDeclaration
    TemplateDeclaration *overroot;      // first in overnext list
    FuncDeclaration *funcroot;          // first function in unified overload list

    Dsymbol *onemember;         // if !=NULL then one member of this template

    d_bool literal;               // this template declaration is a literal
    d_bool ismixin;               // template declaration is only to be used as a mixin
    d_bool isstatic;              // this is static template declaration
    d_bool isTrivialAliasSeq;     // matches `template AliasSeq(T...) { alias AliasSeq = T; }
    d_bool isTrivialAlias;        // matches pattern `template Alias(T) { alias Alias = qualifiers(T); }`
    d_bool deprecated_;           // this template declaration is deprecated
    Visibility visibility;

    TemplatePrevious *previous;         // threaded list of previous instantiation attempts on stack

    TemplateDeclaration *syntaxCopy(Dsymbol *) override;
    bool overloadInsert(Dsymbol *s) override;
    const char *kind() const override;

    Visibility visible() override;


    bool isDeprecated() const override;
    bool isOverloadable() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

/* For type-parameter:
 *  template Foo(ident)             // specType is set to NULL
 *  template Foo(ident : specType)
 * For value-parameter:
 *  template Foo(valType ident)     // specValue is set to NULL
 *  template Foo(valType ident : specValue)
 * For alias-parameter:
 *  template Foo(alias ident)
 * For this-parameter:
 *  template Foo(this ident)
 */
class TemplateParameter : public ASTNode
{
public:
    Loc loc;
    Identifier *ident;

    /* True if this is a part of precedent parameter specialization pattern.
     *
     *  template A(T : X!TL, alias X, TL...) {}
     *  // X and TL are dependent template parameter
     *
     * A dependent template parameter should return MATCHexact in matchArg()
     * to respect the match level of the corresponding precedent parameter.
     */
    d_bool dependent;

    virtual TemplateTypeParameter  *isTemplateTypeParameter();
    virtual TemplateValueParameter *isTemplateValueParameter();
    virtual TemplateAliasParameter *isTemplateAliasParameter();
    virtual TemplateThisParameter *isTemplateThisParameter();
    virtual TemplateTupleParameter *isTemplateTupleParameter();

    virtual TemplateParameter *syntaxCopy() = 0;
    virtual bool declareParameter(Scope *sc) = 0;
    virtual void print(RootObject *oarg, RootObject *oded) = 0;
    virtual RootObject *specialization() = 0;
    virtual RootObject *defaultArg(Loc instLoc, Scope *sc) = 0;
    virtual bool hasDefaultArg() = 0;

    DYNCAST dyncast() const override { return DYNCAST_TEMPLATEPARAMETER; }

    void accept(Visitor *v) override { v->visit(this); }
};

/* Syntax:
 *  ident : specType = defaultType
 */
class TemplateTypeParameter : public TemplateParameter
{
public:
    Type *specType;     // type parameter: if !=NULL, this is the type specialization
    Type *defaultType;

    TemplateTypeParameter *isTemplateTypeParameter() override final;
    TemplateTypeParameter *syntaxCopy() override;
    bool declareParameter(Scope *sc) override final;
    void print(RootObject *oarg, RootObject *oded) override final;
    RootObject *specialization() override final;
    RootObject *defaultArg(Loc instLoc, Scope *sc) override final;
    bool hasDefaultArg() override final;
    void accept(Visitor *v) override { v->visit(this); }
};

/* Syntax:
 *  this ident : specType = defaultType
 */
class TemplateThisParameter final : public TemplateTypeParameter
{
public:
    TemplateThisParameter *isTemplateThisParameter() override;
    TemplateThisParameter *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/* Syntax:
 *  valType ident : specValue = defaultValue
 */
class TemplateValueParameter final : public TemplateParameter
{
public:
    Type *valType;
    Expression *specValue;
    Expression *defaultValue;

    TemplateValueParameter *isTemplateValueParameter() override;
    TemplateValueParameter *syntaxCopy() override;
    bool declareParameter(Scope *sc) override;
    void print(RootObject *oarg, RootObject *oded) override;
    RootObject *specialization() override;
    RootObject *defaultArg(Loc instLoc, Scope *sc) override;
    bool hasDefaultArg() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/* Syntax:
 *  specType ident : specAlias = defaultAlias
 */
class TemplateAliasParameter final : public TemplateParameter
{
public:
    Type *specType;
    RootObject *specAlias;
    RootObject *defaultAlias;

    TemplateAliasParameter *isTemplateAliasParameter() override;
    TemplateAliasParameter *syntaxCopy() override;
    bool declareParameter(Scope *sc) override;
    void print(RootObject *oarg, RootObject *oded) override;
    RootObject *specialization() override;
    RootObject *defaultArg(Loc instLoc, Scope *sc) override;
    bool hasDefaultArg() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/* Syntax:
 *  ident ...
 */
class TemplateTupleParameter final : public TemplateParameter
{
public:
    TemplateTupleParameter *isTemplateTupleParameter() override;
    TemplateTupleParameter *syntaxCopy() override;
    bool declareParameter(Scope *sc) override;
    void print(RootObject *oarg, RootObject *oded) override;
    RootObject *specialization() override;
    RootObject *defaultArg(Loc instLoc, Scope *sc) override;
    bool hasDefaultArg() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/* Given:
 *  foo!(args) =>
 *      name = foo
 *      tiargs = args
 */
class TemplateInstance : public ScopeDsymbol
{
public:
    Identifier *name;

    // Array of Types/Expressions of template
    // instance arguments [int*, char, 10*10]
    Objects *tiargs;

    // Array of Types/Expressions corresponding
    // to TemplateDeclaration.parameters
    // [int, char, 100]
    Objects tdtypes;

    // Modules imported by this template instance
    Modules importedModules;

    Dsymbol *tempdecl;                  // referenced by foo.bar.abc
    Dsymbol *enclosing;                 // if referencing local symbols, this is the context
    Dsymbol *aliasdecl;                 // !=NULL if instance is an alias for its sole member
    TemplateInstance *inst;             // refer to existing instance
    ScopeDsymbol *argsym;               // argument symbol table
    hash_t hash;                        // cached result of toHash()
    Expressions *fargs;                 // for function template, these are the function arguments
    Identifiers *fnames;                // for function template, argument names

    TemplateInstances* deferred;

    Module *memberOf;                   // if !null, then this TemplateInstance appears in memberOf.members[]

    // Used to determine the instance needs code generation.
    // Note that these are inaccurate until semantic analysis phase completed.
    TemplateInstance *tinst;            // enclosing template instance
    TemplateInstance *tnext;            // non-first instantiated instances
    Module *minst;                      // the top module that instantiated this instance

private:
    unsigned short _nest;                // for recursive pretty printing detection, 3 MSBs reserved for flags
public:
    unsigned char inuse;                 // for recursive expansion detection

    TemplateInstance *syntaxCopy(Dsymbol *) override;
    Dsymbol *toAlias() override final;   // resolve real symbol
    const char *kind() const override;
    const char* toPrettyCharsHelper() override final;
    Identifier *getIdent() override final;

    bool isDiscardable();
    bool needsCodegen();

    void accept(Visitor *v) override { v->visit(this); }
};

class TemplateMixin final : public TemplateInstance
{
public:
    TypeQualified *tqual;

    TemplateMixin *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    bool hasPointers() override;

    void accept(Visitor *v) override { v->visit(this); }
};

namespace dmd
{
    // in templateparamsem.d
    bool tpsemantic(TemplateParameter *tp, Scope *sc, TemplateParameters *parameters);

    Expression *isExpression(RootObject *o);
    Dsymbol *isDsymbol(RootObject *o);
    Type *isType(RootObject *o);
    Tuple *isTuple(RootObject *o);
    Parameter *isParameter(RootObject *o);
    TemplateParameter *isTemplateParameter(RootObject *o);
    bool isError(const RootObject *const o);
    void printTemplateStats(bool listInstances, ErrorSink* eSink);
}
