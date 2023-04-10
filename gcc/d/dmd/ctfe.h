
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/ctfe.h
 */

#pragma once

#include "tokens.h"
#include "expression.h"

/**
  A reference to a class, or an interface. We need this when we
  point to a base class (we must record what the type is).
 */
class ClassReferenceExp final : public Expression
{
public:
    StructLiteralExp *value;
    ClassDeclaration *originalClass();

    /// Return index of the field, or -1 if not found
    /// Same as getFieldIndex, but checks for a direct match with the VarDeclaration
    int findFieldIndexByName(VarDeclaration *v);
    void accept(Visitor *v) override { v->visit(this); }
};

/**
  An uninitialized value
 */
class VoidInitExp final : public Expression
{
public:
    VarDeclaration *var;

    const char *toChars() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

/**
  Fake class which holds the thrown exception.
  Used for implementing exception handling.
*/
class ThrownExceptionExp final : public Expression
{
public:
    ClassReferenceExp *thrown; // the thing being tossed
    const char *toChars() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

// This type is only used by the interpreter.

class CTFEExp final : public Expression
{
public:
    const char *toChars() const override;
};
