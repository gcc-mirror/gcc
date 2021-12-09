
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
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

    const char *toChars() const;

    ErrorInitializer   *isErrorInitializer();
    VoidInitializer    *isVoidInitializer();
    StructInitializer  *isStructInitializer();
    ArrayInitializer   *isArrayInitializer();
    ExpInitializer     *isExpInitializer();
    CInitializer       *isCInitializer();

    void accept(Visitor *v) { v->visit(this); }
};

class VoidInitializer : public Initializer
{
public:
    Type *type;         // type that this will initialize to

    void accept(Visitor *v) { v->visit(this); }
};

class ErrorInitializer : public Initializer
{
public:
    void accept(Visitor *v) { v->visit(this); }
};

class StructInitializer : public Initializer
{
public:
    Identifiers field;  // of Identifier *'s
    Initializers value; // parallel array of Initializer *'s

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

    bool isAssociativeArray() const;
    Expression *toAssocArrayLiteral();

    void accept(Visitor *v) { v->visit(this); }
};

class ExpInitializer : public Initializer
{
public:
    bool expandTuples;
    Expression *exp;

    void accept(Visitor *v) { v->visit(this); }
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

class CInitializer : public Initializer
{
public:
    DesigInits initializerList;
    Type *type;         // type that array will be used to initialize
    bool sem;           // true if semantic() is run

    void accept(Visitor *v) { v->visit(this); }
};

Expression *initializerToExpression(Initializer *init, Type *t = NULL);
