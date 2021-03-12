
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by KennyTM
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/intrange.h
 */

#pragma once

#include "globals.h"   // for uinteger_t
class Type;
class Expression;

/**
This class represents a "sign-extended number", i.e. a 65-bit number, which can
represent all built-in integer types in D. This class is mainly used for
performing value-range propagation only, therefore all arithmetic are done with
saturation, not wrapping as usual.
*/
struct SignExtendedNumber
{
    /// The lower 64-bit of the number.
    uinteger_t value;
    /// The sign (i.e. the most significant bit) of the number.
    bool negative;

    /// Create an uninitialized sign-extended number.
    SignExtendedNumber() {}

    /// Create a sign-extended number from an unsigned 64-bit number.
    SignExtendedNumber(uinteger_t value_)
        : value(value_), negative(false) {}
    /// Create a sign-extended number from the lower 64-bit and the sign bit.
    SignExtendedNumber(uinteger_t value_, bool negative_)
        : value(value_), negative(negative_) {}

    /// Create a sign-extended number from a signed 64-bit number.
    static SignExtendedNumber fromInteger(uinteger_t value_);

    /// Get the minimum or maximum value of a sign-extended number.
    static SignExtendedNumber extreme(bool minimum);

    // These names probably shouldn't be used anyway, as they are common macros
#undef max
#undef min
    static SignExtendedNumber max();
    static SignExtendedNumber min() { return SignExtendedNumber(0, true); }

    /// Check if the sign-extended number is minimum or zero.
    bool isMinimum() const { return negative && value == 0; }

    /// Compare two sign-extended number.
    bool operator==(const SignExtendedNumber&) const;
    bool operator!=(const SignExtendedNumber& a) const { return !(*this == a); }
    bool operator<(const SignExtendedNumber&) const;
    bool operator>(const SignExtendedNumber& a) const { return a < *this; }
    bool operator<=(const SignExtendedNumber& a) const { return !(a < *this); }
    bool operator>=(const SignExtendedNumber& a) const { return !(*this < a); }

    /// Increase the sign-extended number by 1 (saturated).
    SignExtendedNumber& operator++();
    /// Compute the saturated complement of a sign-extended number.
    SignExtendedNumber operator~() const;
    /// Compute the saturated negation of a sign-extended number.
    SignExtendedNumber operator-() const;

    /// Compute the saturated binary and of two sign-extended number.
    SignExtendedNumber operator&(const SignExtendedNumber&) const;
    /// Compute the saturated binary or of two sign-extended number.
    SignExtendedNumber operator|(const SignExtendedNumber&) const;
    /// Compute the saturated binary xor of two sign-extended number.
    SignExtendedNumber operator^(const SignExtendedNumber&) const;
    /// Compute the saturated sum of two sign-extended number.
    SignExtendedNumber operator+(const SignExtendedNumber&) const;
    /// Compute the saturated difference of two sign-extended number.
    SignExtendedNumber operator-(const SignExtendedNumber&) const;
    /// Compute the saturated product of two sign-extended number.
    SignExtendedNumber operator*(const SignExtendedNumber&) const;
    /// Compute the saturated quotient of two sign-extended number.
    SignExtendedNumber operator/(const SignExtendedNumber&) const;
    /// Compute the saturated modulus of two sign-extended number.
    SignExtendedNumber operator%(const SignExtendedNumber&) const;

    /// Compute the saturated shifts of two sign-extended number.
    SignExtendedNumber operator<<(const SignExtendedNumber&) const;
    SignExtendedNumber operator>>(const SignExtendedNumber&) const;
};

/**
This class represents a range of integers, denoted by its lower and upper bounds
(inclusive).
*/
struct IntRange
{
    SignExtendedNumber imin, imax;

    /// Create an uninitialized range.
    IntRange() {}

    /// Create a range consisting of a single number.
    IntRange(const SignExtendedNumber& a)
        : imin(a), imax(a) {}
    /// Create a range with the lower and upper bounds.
    IntRange(const SignExtendedNumber& lower, const SignExtendedNumber& upper)
        : imin(lower), imax(upper) {}

    /// Create the tightest range containing all valid integers in the specified
    /// type.
    static IntRange fromType(Type *type);
    /// Create the tightest range containing all valid integers in the type with
    /// a forced signedness.
    static IntRange fromType(Type *type, bool isUnsigned);


    /// Create the tightest range containing all specified numbers.
    static IntRange fromNumbers2(const SignExtendedNumber numbers[2]);
    static IntRange fromNumbers4(const SignExtendedNumber numbers[4]);

    /// Create the widest range possible.
    static IntRange widest();

    /// Cast the integer range to a signed type with the given size mask.
    IntRange& castSigned(uinteger_t mask);
    /// Cast the integer range to an unsigned type with the given size mask.
    IntRange& castUnsigned(uinteger_t mask);
    /// Cast the integer range to the dchar type.
    IntRange& castDchar();

    /// Cast the integer range to a specific type.
    IntRange& cast(Type *type);
    /// Cast the integer range to a specific type, forcing it to be unsigned.
    IntRange& castUnsigned(Type *type);

    /// Check if this range contains another range.
    bool contains(const IntRange& a) const;

    /// Check if this range contains 0.
    bool containsZero() const;

    /// Compute the range of the negated absolute values of the original range.
    IntRange absNeg() const;

    /// Compute the union of two ranges.
    IntRange unionWith(const IntRange& other) const;
    void unionOrAssign(const IntRange& other, bool& union_);

    /// Dump the content of the integer range to the console.
    const IntRange& dump(const char* funcName, Expression *e) const;

    /// Split the range into two nonnegative- and negative-only subintervals.
    void splitBySign(IntRange& negRange, bool& hasNegRange,
                     IntRange& nonNegRange, bool& hasNonNegRange) const;

    /// Credits to Timon Gehr maxOr, minOr, maxAnd, minAnd
    /// https://github.com/tgehr/d-compiler/blob/master/vrange.d
    static SignExtendedNumber maxOr(const IntRange&, const IntRange&);
    static SignExtendedNumber minOr(const IntRange&, const IntRange&);
    static SignExtendedNumber maxAnd(const IntRange&, const IntRange&);
    static SignExtendedNumber minAnd(const IntRange&, const IntRange&);
    static void swap(IntRange&, IntRange&);

    IntRange operator~() const;
    IntRange operator-() const;
    IntRange operator&(const IntRange&) const;
    IntRange operator|(const IntRange&) const;
    IntRange operator^(const IntRange&) const;
    IntRange operator+(const IntRange&) const;
    IntRange operator-(const IntRange&) const;
    IntRange operator*(const IntRange&) const;
    IntRange operator/(const IntRange&) const;
    IntRange operator%(const IntRange&) const;
    IntRange operator<<(const IntRange&) const;
    IntRange operator>>(const IntRange&) const;
};
