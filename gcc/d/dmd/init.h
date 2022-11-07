
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/init.h
 */

#pragma once

#include "ast_node.h"
#include "globals.h"
#include "arraytypes.h"
#include "visitor.h"

class Identifier;
class Expression;
class Type;
class ErrorInitializer;
class VoidInitializer;
class StructInitializer;
class ArrayInitializer;
class ExpInitializer;
class CInitializer;

enum NeedInterpret { INITnointerpret, INITinterpret };

class Initializer : public ASTNode
{
public:
    Loc loc;
    unsigned char kind;

    DYNCAST dyncast() const override { return DYNCAST_INITIALIZER; }

    const char *toChars() const override final;

    ErrorInitializer   *isErrorInitializer();
    VoidInitializer    *isVoidInitializer();
    StructInitializer  *isStructInitializer();
    ArrayInitializer   *isArrayInitializer();
    ExpInitializer     *isExpInitializer();
    CInitializer       *isCInitializer();

    void accept(Visitor *v) override { v->visit(this); }
};

class VoidInitializer final : public Initializer
{
public:
    Type *type;         // type that this will initialize to

    void accept(Visitor *v) override { v->visit(this); }
};

class ErrorInitializer final : public Initializer
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class StructInitializer final : public Initializer
{
public:
    Identifiers field;  // of Identifier *'s
    Initializers value; // parallel array of Initializer *'s

    void accept(Visitor *v) override { v->visit(this); }
};

class ArrayInitializer final : public Initializer
{
public:
    Expressions index;  // indices
    Initializers value; // of Initializer *'s
    unsigned dim;       // length of array being initialized
    Type *type;         // type that array will be used to initialize
    bool sem;           // true if semantic() is run
    bool isCarray;      // C array semantics

    bool isAssociativeArray() const;
    Expression *toAssocArrayLiteral();

    void accept(Visitor *v) override { v->visit(this); }
};

class ExpInitializer final : public Initializer
{
public:
    bool expandTuples;
    Expression *exp;

    void accept(Visitor *v) override { v->visit(this); }
};

struct Designator
{
    Expression *exp;
    Identifier *ident;
};

struct DesigInit
{
    Designators *designatorList;
    Initializer *initializer;
};

class CInitializer final : public Initializer
{
public:
    DesigInits initializerList;
    Type *type;         // type that array will be used to initialize
    bool sem;           // true if semantic() is run

    void accept(Visitor *v) override { v->visit(this); }
};

Expression *initializerToExpression(Initializer *init, Type *t = NULL, const bool isCfile = false);
