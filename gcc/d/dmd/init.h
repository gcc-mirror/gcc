
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/init.h
 */

#pragma once

#include "root/root.h"
#include "ast_node.h"
#include "globals.h"
#include "arraytypes.h"
#include "visitor.h"

class Identifier;
class Expression;
struct Scope;
class Type;
class AggregateDeclaration;
class Initializer;
class ErrorInitializer;
class VoidInitializer;
class StructInitializer;
class ArrayInitializer;
class ExpInitializer;

enum NeedInterpret { INITnointerpret, INITinterpret };

Initializer *initializerSemantic(Initializer *init, Scope *sc, Type *t, NeedInterpret needInterpret);

class Initializer : public ASTNode
{
public:
    Loc loc;

    Initializer(Loc loc);
    virtual Initializer *syntaxCopy() = 0;
    static Initializers *arraySyntaxCopy(Initializers *ai);

    const char *toChars();

    virtual ErrorInitializer   *isErrorInitializer() { return NULL; }
    virtual VoidInitializer    *isVoidInitializer() { return NULL; }
    virtual StructInitializer  *isStructInitializer()  { return NULL; }
    virtual ArrayInitializer   *isArrayInitializer()  { return NULL; }
    virtual ExpInitializer     *isExpInitializer()  { return NULL; }
    void accept(Visitor *v) { v->visit(this); }
};

class VoidInitializer : public Initializer
{
public:
    Type *type;         // type that this will initialize to

    VoidInitializer(Loc loc);
    Initializer *syntaxCopy();

    virtual VoidInitializer *isVoidInitializer() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class ErrorInitializer : public Initializer
{
public:
    ErrorInitializer();
    Initializer *syntaxCopy();

    virtual ErrorInitializer *isErrorInitializer() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class StructInitializer : public Initializer
{
public:
    Identifiers field;  // of Identifier *'s
    Initializers value; // parallel array of Initializer *'s

    StructInitializer(Loc loc);
    Initializer *syntaxCopy();
    void addInit(Identifier *field, Initializer *value);

    StructInitializer *isStructInitializer() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class ArrayInitializer : public Initializer
{
public:
    Expressions index;  // indices
    Initializers value; // of Initializer *'s
    unsigned dim;       // length of array being initialized
    Type *type;         // type that array will be used to initialize
    bool sem;           // true if semantic() is run

    ArrayInitializer(Loc loc);
    Initializer *syntaxCopy();
    void addInit(Expression *index, Initializer *value);
    bool isAssociativeArray();
    Expression *toAssocArrayLiteral();

    ArrayInitializer *isArrayInitializer() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class ExpInitializer : public Initializer
{
public:
    Expression *exp;
    bool expandTuples;

    ExpInitializer(Loc loc, Expression *exp);
    Initializer *syntaxCopy();

    ExpInitializer *isExpInitializer() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

Expression *initializerToExpression(Initializer *init, Type *t = NULL);
