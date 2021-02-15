
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/ctfe.h
 */

#pragma once

#include "arraytypes.h"
#include "tokens.h"
#include "expression.h"

/**
   Global status of the CTFE engine. Mostly used for performance diagnostics
 */
struct CtfeStatus
{
    static int callDepth; // current number of recursive calls
    /* When printing a stack trace,
     * suppress this number of calls
     */
    static int stackTraceCallsToSuppress;
    static int maxCallDepth; // highest number of recursive calls
    static int numArrayAllocs; // Number of allocated arrays
    static int numAssignments; // total number of assignments executed
};

/**
  A reference to a class, or an interface. We need this when we
  point to a base class (we must record what the type is).
 */
class ClassReferenceExp : public Expression
{
public:
    StructLiteralExp *value;
    ClassReferenceExp(Loc loc, StructLiteralExp *lit, Type *type);
    ClassDeclaration *originalClass();

    /// Return index of the field, or -1 if not found
    int getFieldIndex(Type *fieldtype, unsigned fieldoffset);
    /// Return index of the field, or -1 if not found
    /// Same as getFieldIndex, but checks for a direct match with the VarDeclaration
    int findFieldIndexByName(VarDeclaration *v);
    void accept(Visitor *v) { v->visit(this); }
};

// The various functions are used only to detect compiler CTFE bugs
Expression *getValue(VarDeclaration *vd);
bool hasValue(VarDeclaration *vd);
void setValueNull(VarDeclaration *vd);
void setValueWithoutChecking(VarDeclaration *vd, Expression *newval);
void setValue(VarDeclaration *vd, Expression *newval);

/// Return index of the field, or -1 if not found
/// Same as getFieldIndex, but checks for a direct match with the VarDeclaration
int findFieldIndexByName(StructDeclaration *sd, VarDeclaration *v);


/** An uninitialized value
 */
class VoidInitExp : public Expression
{
public:
    VarDeclaration *var;

    VoidInitExp(VarDeclaration *var, Type *type);
    const char *toChars();
    void accept(Visitor *v) { v->visit(this); }
};

// Create an appropriate void initializer
UnionExp voidInitLiteral(Type *t, VarDeclaration *var);

/** Fake class which holds the thrown exception.
    Used for implementing exception handling.
*/
class ThrownExceptionExp : public Expression
{
public:
    ClassReferenceExp *thrown; // the thing being tossed
    ThrownExceptionExp(Loc loc, ClassReferenceExp *victim);
    const char *toChars();
    /// Generate an error message when this exception is not caught
    void generateUncaughtError();
    void accept(Visitor *v) { v->visit(this); }
};

/****************************************************************/

// This type is only used by the interpreter.

class CTFEExp : public Expression
{
public:
    CTFEExp(TOK tok);

    const char *toChars();

    // Handy instances to share
    static CTFEExp *cantexp;
    static CTFEExp *voidexp;
    static CTFEExp *breakexp;
    static CTFEExp *continueexp;
    static CTFEExp *gotoexp;

    static bool isCantExp(Expression *e) { return e && e->op == TOKcantexp; }
    static bool isGotoExp(Expression *e) { return e && e->op == TOKgoto; }
};

/****************************************************************/


/// True if 'e' is TOKcantexp, or an exception
bool exceptionOrCantInterpret(Expression *e);

// Used for debugging only
void showCtfeExpr(Expression *e, int level = 0);

/// Return true if this is a valid CTFE expression
bool isCtfeValueValid(Expression *newval);
bool isCtfeReferenceValid(Expression *newval);

/// Given expr, which evaluates to an array/AA/string literal,
/// return true if it needs to be copied
bool needToCopyLiteral(Expression *expr);

/// Make a copy of the ArrayLiteral, AALiteral, String, or StructLiteral.
/// This value will be used for in-place modification.
UnionExp copyLiteral(Expression *e);

/// Set this literal to the given type, copying it if necessary
Expression *paintTypeOntoLiteral(Type *type, Expression *lit);
Expression *paintTypeOntoLiteral(UnionExp *pue, Type *type, Expression *lit);
UnionExp paintTypeOntoLiteralCopy(Type *type, Expression *lit);

/// Convert from a CTFE-internal slice, into a normal Expression
Expression *resolveSlice(Expression *e, UnionExp *pue = NULL);

/// Determine the array length, without interpreting the expression.
uinteger_t resolveArrayLength(Expression *e);

/// Create an array literal consisting of 'elem' duplicated 'dim' times.
ArrayLiteralExp *createBlockDuplicatedArrayLiteral(UnionExp *pue, Loc loc, Type *type,
        Expression *elem, size_t dim);

/// Create a string literal consisting of 'value' duplicated 'dim' times.
StringExp *createBlockDuplicatedStringLiteral(UnionExp *pue, Loc loc, Type *type,
        unsigned value, size_t dim, unsigned char sz);


/* Set dest = src, where both dest and src are container value literals
 * (ie, struct literals, or static arrays (can be an array literal or a string)
 * Assignment is recursively in-place.
 * Purpose: any reference to a member of 'dest' will remain valid after the
 * assignment.
 */
void assignInPlace(Expression *dest, Expression *src);

/// Duplicate the elements array, then set field 'indexToChange' = newelem.
Expressions *changeOneElement(Expressions *oldelems, size_t indexToChange, Expression *newelem);

/// Given an AA literal aae,  set arr[index] = newval and return the new array.
Expression *assignAssocArrayElement(Loc loc, AssocArrayLiteralExp *aae,
    Expression *index, Expression *newval);

/// Given array literal oldval of type ArrayLiteralExp or StringExp, of length
/// oldlen, change its length to newlen. If the newlen is longer than oldlen,
/// all new elements will be set to the default initializer for the element type.
UnionExp changeArrayLiteralLength(Loc loc, TypeArray *arrayType,
    Expression *oldval,  size_t oldlen, size_t newlen);



/// Return true if t is a pointer (not a function pointer)
bool isPointer(Type *t);

// For CTFE only. Returns true if 'e' is TRUE or a non-null pointer.
bool isTrueBool(Expression *e);

/// Is it safe to convert from srcPointee* to destPointee* ?
///  srcPointee is the genuine type (never void).
///  destPointee may be void.
bool isSafePointerCast(Type *srcPointee, Type *destPointee);

/// Given pointer e, return the memory block expression it points to,
/// and set ofs to the offset within that memory block.
Expression *getAggregateFromPointer(Expression *e, dinteger_t *ofs);

/// Return true if agg1 and agg2 are pointers to the same memory block
bool pointToSameMemoryBlock(Expression *agg1, Expression *agg2);

// return e1 - e2 as an integer, or error if not possible
UnionExp pointerDifference(Loc loc, Type *type, Expression *e1, Expression *e2);

/// Return 1 if true, 0 if false
/// -1 if comparison is illegal because they point to non-comparable memory blocks
int comparePointers(TOK op, Expression *agg1, dinteger_t ofs1, Expression *agg2, dinteger_t ofs2);

// Return eptr op e2, where eptr is a pointer, e2 is an integer,
// and op is TOKadd or TOKmin
UnionExp pointerArithmetic(Loc loc, TOK op, Type *type,
    Expression *eptr, Expression *e2);

// True if conversion from type 'from' to 'to' involves a reinterpret_cast
// floating point -> integer or integer -> floating point
bool isFloatIntPaint(Type *to, Type *from);

// Reinterpret float/int value 'fromVal' as a float/integer of type 'to'.
Expression *paintFloatInt(UnionExp *pue, Expression *fromVal, Type *to);

/// Return true if t is an AA
bool isAssocArray(Type *t);

/// Given a template AA type, extract the corresponding built-in AA type
TypeAArray *toBuiltinAAType(Type *t);

/*  Given an AA literal 'ae', and a key 'e2':
 *  Return ae[e2] if present, or NULL if not found.
 *  Return TOKcantexp on error.
 */
Expression *findKeyInAA(Loc loc, AssocArrayLiteralExp *ae, Expression *e2);

/// True if type is TypeInfo_Class
bool isTypeInfo_Class(Type *type);


/***********************************************
      COW const-folding operations
***********************************************/

/// Return true if non-pointer expression e can be compared
/// with >,is, ==, etc, using ctfeCmp, ctfeEquals, ctfeIdentity
bool isCtfeComparable(Expression *e);

/// Evaluate ==, !=.  Resolves slices before comparing. Returns 0 or 1
int ctfeEqual(Loc loc, TOK op, Expression *e1, Expression *e2);

/// Evaluate is, !is.  Resolves slices before comparing. Returns 0 or 1
int ctfeIdentity(Loc loc, TOK op, Expression *e1, Expression *e2);

/// Returns rawCmp OP 0; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int specificCmp(TOK op, int rawCmp);

/// Returns e1 OP e2; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int intUnsignedCmp(TOK op, dinteger_t n1, dinteger_t n2);

/// Returns e1 OP e2; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int intSignedCmp(TOK op, sinteger_t n1, sinteger_t n2);

/// Returns e1 OP e2; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int realCmp(TOK op, real_t r1, real_t r2);

/// Evaluate >,<=, etc. Resolves slices before comparing. Returns 0 or 1
int ctfeCmp(Loc loc, TOK op, Expression *e1, Expression *e2);

/// Returns e1 ~ e2. Resolves slices before concatenation.
UnionExp ctfeCat(Loc loc, Type *type, Expression *e1, Expression *e2);

/// Same as for constfold.Index, except that it only works for static arrays,
/// dynamic arrays, and strings.
Expression *ctfeIndex(Loc loc, Type *type, Expression *e1, uinteger_t indx);

/// Cast 'e' of type 'type' to type 'to'.
Expression *ctfeCast(UnionExp *pue, Loc loc, Type *type, Type *to, Expression *e);
