
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by KennyTM
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/intrange.c
 */

#include "root/dsystem.h"

#include "intrange.h"
#include "mars.h"
#include "mtype.h"
#include "expression.h"

// Copy the sign to the value *x*. Equivalent to `sign ? -x : x`.
static uinteger_t copySign(uinteger_t x, bool sign)
{
    // return sign ? -x : x;
    return (x - (uinteger_t)sign) ^ -(uinteger_t)sign;
}

#ifndef UINT64_MAX
#define UINT64_MAX 0xFFFFFFFFFFFFFFFFULL
#endif

//==================== SignExtendedNumber ======================================

SignExtendedNumber SignExtendedNumber::fromInteger(uinteger_t value_)
{
    return SignExtendedNumber(value_, value_ >> 63);
}

bool SignExtendedNumber::operator==(const SignExtendedNumber& a) const
{
    return value == a.value && negative == a.negative;
}

bool SignExtendedNumber::operator<(const SignExtendedNumber& a) const
{
    return (negative && !a.negative)
        || (negative == a.negative && value < a.value);
}

SignExtendedNumber SignExtendedNumber::extreme(bool minimum)
{
    return SignExtendedNumber(minimum-1, minimum);
}

SignExtendedNumber SignExtendedNumber::max()
{
    return SignExtendedNumber(UINT64_MAX, false);
}

SignExtendedNumber& SignExtendedNumber::operator++()
{
    if (value != UINT64_MAX)
        ++value;
    else if (negative)
    {
        value = 0;
        negative = false;
    }
    return *this;
}

SignExtendedNumber SignExtendedNumber::operator~() const
{
    if (~value == 0)
        return SignExtendedNumber(~value);
    else
        return SignExtendedNumber(~value, !negative);
}

SignExtendedNumber SignExtendedNumber::operator-() const
{
    if (value == 0)
        return SignExtendedNumber(-negative);
    else
        return SignExtendedNumber(-value, !negative);
}

SignExtendedNumber SignExtendedNumber::operator&(const SignExtendedNumber& rhs) const
{
    return SignExtendedNumber(value & rhs.value);
}

SignExtendedNumber SignExtendedNumber::operator|(const SignExtendedNumber& rhs) const
{
    return SignExtendedNumber(value | rhs.value);
}

SignExtendedNumber SignExtendedNumber::operator^(const SignExtendedNumber& rhs) const
{
    return SignExtendedNumber(value ^ rhs.value);
}

SignExtendedNumber SignExtendedNumber::operator+(const SignExtendedNumber& rhs) const
{
    uinteger_t sum = value + rhs.value;
    bool carry = sum < value && sum < rhs.value;
    if (negative != rhs.negative)
        return SignExtendedNumber(sum, !carry);
    else if (negative)
        return SignExtendedNumber(carry ? sum : 0, true);
    else
        return SignExtendedNumber(carry ? UINT64_MAX : sum, false);
}

SignExtendedNumber SignExtendedNumber::operator-(const SignExtendedNumber& rhs) const
{
    if (rhs.isMinimum())
        return negative ? SignExtendedNumber(value, false) : max();
    else
        return *this + (-rhs);
}

SignExtendedNumber SignExtendedNumber::operator*(const SignExtendedNumber& rhs) const
{
    // perform *saturated* multiplication, otherwise we may get bogus ranges
    //  like 0x10 * 0x10 == 0x100 == 0.

    /* Special handling for zeros:
        INT65_MIN * 0 = 0
        INT65_MIN * + = INT65_MIN
        INT65_MIN * - = INT65_MAX
        0 * anything = 0
    */
    if (value == 0)
    {
        if (!negative)
            return *this;
        else if (rhs.negative)
            return max();
        else
            return rhs.value == 0 ? rhs : *this;
    }
    else if (rhs.value == 0)
        return rhs * *this;   // don't duplicate the symmetric case.

    SignExtendedNumber rv;
    // these are != 0 now surely.
    uinteger_t tAbs = copySign(value, negative);
    uinteger_t aAbs = copySign(rhs.value, rhs.negative);
    rv.negative = negative != rhs.negative;
    if (UINT64_MAX / tAbs < aAbs)
        rv.value = rv.negative-1;
    else
        rv.value = copySign(tAbs * aAbs, rv.negative);
    return rv;
}

SignExtendedNumber SignExtendedNumber::operator/(const SignExtendedNumber& rhs) const
{
    /* special handling for zeros:
        INT65_MIN / INT65_MIN = 1
        anything / INT65_MIN = 0
        + / 0 = INT65_MAX  (eh?)
        - / 0 = INT65_MIN  (eh?)
    */
    if (rhs.value == 0)
    {
        if (rhs.negative)
            return SignExtendedNumber(value == 0 && negative);
        else
            return extreme(negative);
    }

    uinteger_t aAbs = copySign(rhs.value, rhs.negative);
    uinteger_t rvVal;

    if (!isMinimum())
        rvVal = copySign(value, negative) / aAbs;
    // Special handling for INT65_MIN
    //  if the denominator is not a power of 2, it is same as UINT64_MAX / x.
    else if (aAbs & (aAbs-1))
        rvVal = UINT64_MAX / aAbs;
    // otherwise, it's the same as reversing the bits of x.
    else
    {
        if (aAbs == 1)
            return extreme(!rhs.negative);
        rvVal = 1ULL << 63;
        aAbs >>= 1;
        if (aAbs & 0xAAAAAAAAAAAAAAAAULL) rvVal >>= 1;
        if (aAbs & 0xCCCCCCCCCCCCCCCCULL) rvVal >>= 2;
        if (aAbs & 0xF0F0F0F0F0F0F0F0ULL) rvVal >>= 4;
        if (aAbs & 0xFF00FF00FF00FF00ULL) rvVal >>= 8;
        if (aAbs & 0xFFFF0000FFFF0000ULL) rvVal >>= 16;
        if (aAbs & 0xFFFFFFFF00000000ULL) rvVal >>= 32;
    }
    bool rvNeg = negative != rhs.negative;
    rvVal = copySign(rvVal, rvNeg);

    return SignExtendedNumber(rvVal, rvVal != 0 && rvNeg);
}

SignExtendedNumber SignExtendedNumber::operator%(const SignExtendedNumber& rhs) const
{
    if (rhs.value == 0)
        return !rhs.negative ? rhs : isMinimum() ? SignExtendedNumber(0) : *this;

    uinteger_t aAbs = copySign(rhs.value, rhs.negative);
    uinteger_t rvVal;

    // a % b == sgn(a) * abs(a) % abs(b).
    if (!isMinimum())
        rvVal = copySign(value, negative) % aAbs;
    // Special handling for INT65_MIN
    //  if the denominator is not a power of 2, it is same as UINT64_MAX%x + 1.
    else if (aAbs & (aAbs - 1))
        rvVal = UINT64_MAX % aAbs + 1;
    //  otherwise, the modulus is trivially zero.
    else
        rvVal = 0;

    rvVal = copySign(rvVal, negative);
    return SignExtendedNumber(rvVal, rvVal != 0 && negative);
}

SignExtendedNumber SignExtendedNumber::operator<<(const SignExtendedNumber& rhs) const
{
    // assume left-shift the shift-amount is always unsigned. Thus negative
    //  shifts will give huge result.
    if (value == 0)
        return *this;
    else if (rhs.negative)
        return extreme(negative);

    uinteger_t v = copySign(value, negative);

    // compute base-2 log of 'v' to determine the maximum allowed bits to shift.
    // Ref: http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog

    // Why is this a size_t? Looks like a bug.
    size_t r, s;

    r = (v > 0xFFFFFFFFULL) << 5; v >>= r;
    s = (v > 0xFFFFULL    ) << 4; v >>= s; r |= s;
    s = (v > 0xFFULL      ) << 3; v >>= s; r |= s;
    s = (v > 0xFULL       ) << 2; v >>= s; r |= s;
    s = (v > 0x3ULL       ) << 1; v >>= s; r |= s;
                                           r |= (v >> 1);

    uinteger_t allowableShift = 63 - r;
    if (rhs.value > allowableShift)
        return extreme(negative);
    else
        return SignExtendedNumber(value << rhs.value, negative);
}

SignExtendedNumber SignExtendedNumber::operator>>(const SignExtendedNumber& rhs) const
{
    if (rhs.negative || rhs.value > 63)
        return negative ? SignExtendedNumber(-1, true) : SignExtendedNumber(0);
    else if (isMinimum())
        return rhs.value == 0 ? *this : SignExtendedNumber(-1ULL << (64 - rhs.value), true);

    uinteger_t x = value ^ -negative;
    x >>= rhs.value;
    return SignExtendedNumber(x ^ -negative, negative);
}


//==================== IntRange ================================================

IntRange IntRange::widest()
{
    return IntRange(SignExtendedNumber::min(), SignExtendedNumber::max());
}

IntRange IntRange::fromType(Type *type)
{
    return fromType(type, type->isunsigned());
}

IntRange IntRange::fromType(Type *type, bool isUnsigned)
{
    if (!type->isintegral() || type->toBasetype()->ty == Tvector)
        return widest();

    uinteger_t mask = type->sizemask();
    SignExtendedNumber lower(0), upper(mask);
    if (type->toBasetype()->ty == Tdchar)
        upper.value = 0x10FFFFULL;
    else if (!isUnsigned)
    {
        lower.value = ~(mask >> 1);
        lower.negative = true;
        upper.value = (mask >> 1);
    }
    return IntRange(lower, upper);
}

IntRange IntRange::fromNumbers2(const SignExtendedNumber numbers[2])
{
    if (numbers[0] < numbers[1])
        return IntRange(numbers[0], numbers[1]);
    else
        return IntRange(numbers[1], numbers[0]);
}
IntRange IntRange::fromNumbers4(const SignExtendedNumber numbers[4])
{
    IntRange ab = fromNumbers2(numbers);
    IntRange cd = fromNumbers2(numbers + 2);
    if (cd.imin < ab.imin)
        ab.imin = cd.imin;
    if (cd.imax > ab.imax)
        ab.imax = cd.imax;
    return ab;
}

bool IntRange::contains(const IntRange& a) const
{
    return imin <= a.imin && imax >= a.imax;
}

bool IntRange::containsZero() const
{
    return (imin.negative && !imax.negative)
        || (!imin.negative && imin.value == 0);
}

IntRange& IntRange::castUnsigned(uinteger_t mask)
{
    // .... 0x1eff ] [0x1f00 .. 0x1fff] [0 .. 0xff] [0x100 .. 0x1ff] [0x200 ....
    //
    // regular unsigned type. We just need to see if ir steps across the
    //  boundary of validRange. If yes, ir will represent the whole validRange,
    //  otherwise, we just take the modulus.
    // e.g. [0x105, 0x107] & 0xff == [5, 7]
    //      [0x105, 0x207] & 0xff == [0, 0xff]
    uinteger_t minChunk = imin.value & ~mask;
    uinteger_t maxChunk = imax.value & ~mask;
    if (minChunk == maxChunk && imin.negative == imax.negative)
    {
        imin.value &= mask;
        imax.value &= mask;
    }
    else
    {
        imin.value = 0;
        imax.value = mask;
    }
    imin.negative = imax.negative = false;
    return *this;
}

IntRange& IntRange::castSigned(uinteger_t mask)
{
    // .... 0x1e7f ] [0x1e80 .. 0x1f7f] [0x1f80 .. 0x7f] [0x80 .. 0x17f] [0x180 ....
    //
    // regular signed type. We use a technique similar to the unsigned version,
    //  but the chunk has to be offset by 1/2 of the range.
    uinteger_t halfChunkMask = mask >> 1;
    uinteger_t minHalfChunk = imin.value & ~halfChunkMask;
    uinteger_t maxHalfChunk = imax.value & ~halfChunkMask;
    int minHalfChunkNegativity = imin.negative; // 1 = neg, 0 = nonneg, -1 = chunk containing ::max
    int maxHalfChunkNegativity = imax.negative;
    if (minHalfChunk & mask)
    {
        minHalfChunk += halfChunkMask+1;
        if (minHalfChunk == 0)
            -- minHalfChunkNegativity;
    }
    if (maxHalfChunk & mask)
    {
        maxHalfChunk += halfChunkMask+1;
        if (maxHalfChunk == 0)
            -- maxHalfChunkNegativity;
    }
    if (minHalfChunk == maxHalfChunk && minHalfChunkNegativity == maxHalfChunkNegativity)
    {
        imin.value &= mask;
        imax.value &= mask;
        // sign extend if necessary.
        imin.negative = imin.value & ~halfChunkMask;
        imax.negative = imax.value & ~halfChunkMask;
        halfChunkMask += 1;
        imin.value = (imin.value ^ halfChunkMask) - halfChunkMask;
        imax.value = (imax.value ^ halfChunkMask) - halfChunkMask;
    }
    else
    {
        imin = SignExtendedNumber(~halfChunkMask, true);
        imax = SignExtendedNumber(halfChunkMask, false);
    }
    return *this;
}

IntRange& IntRange::castDchar()
{
    // special case for dchar. Casting to dchar means "I'll ignore all
    //  invalid characters."
    castUnsigned(0xFFFFFFFFULL);
    if (imin.value > 0x10FFFFULL)   // ??
        imin.value = 0x10FFFFULL;   // ??
    if (imax.value > 0x10FFFFULL)
        imax.value = 0x10FFFFULL;
    return *this;
}

IntRange& IntRange::cast(Type *type)
{
    if (!type->isintegral() || type->toBasetype()->ty == Tvector)
        return *this;
    else if (!type->isunsigned())
        return castSigned(type->sizemask());
    else if (type->toBasetype()->ty == Tdchar)
        return castDchar();
    else
        return castUnsigned(type->sizemask());
}

IntRange& IntRange::castUnsigned(Type *type)
{
    if (!type->isintegral() || type->toBasetype()->ty == Tvector)
        return castUnsigned(UINT64_MAX);
    else if (type->toBasetype()->ty == Tdchar)
        return castDchar();
    else
        return castUnsigned(type->sizemask());
}

IntRange IntRange::absNeg() const
{
    if (imax.negative)
        return *this;
    else if (!imin.negative)
        return IntRange(-imax, -imin);
    else
    {
        SignExtendedNumber imaxAbsNeg = -imax;
        return IntRange(imaxAbsNeg < imin ? imaxAbsNeg : imin,
                        SignExtendedNumber(0));
    }
}

IntRange IntRange::unionWith(const IntRange& other) const
{
    return IntRange(imin < other.imin ? imin : other.imin,
                    imax > other.imax ? imax : other.imax);
}

void IntRange::unionOrAssign(const IntRange& other, bool& union_)
{
    if (!union_ || imin > other.imin)
        imin = other.imin;
    if (!union_ || imax < other.imax)
        imax = other.imax;
    union_ = true;
}

void IntRange::splitBySign(IntRange& negRange, bool& hasNegRange,
                           IntRange& nonNegRange, bool& hasNonNegRange) const
{
    hasNegRange = imin.negative;
    if (hasNegRange)
    {
        negRange.imin = imin;
        negRange.imax = imax.negative ? imax : SignExtendedNumber(-1, true);
    }
    hasNonNegRange = !imax.negative;
    if (hasNonNegRange)
    {
        nonNegRange.imin = imin.negative ? SignExtendedNumber(0) : imin;
        nonNegRange.imax = imax;
    }
}

IntRange IntRange::operator~() const
{
    return IntRange(~imax, ~imin);
}

IntRange IntRange::operator-() const
{
    return IntRange(-imax, -imin);
}

IntRange IntRange::operator&(const IntRange& rhs) const
{
    // unsigned or identical sign bits
    if ((imin.negative ^ imax.negative) != 1 && (rhs.imin.negative ^ rhs.imax.negative) != 1)
    {
        return IntRange(minAnd(*this, rhs), maxAnd(*this, rhs));
    }

    IntRange l = IntRange(*this);
    IntRange r = IntRange(rhs);

    // both intervals span [-1,0]
    if ((l.imin.negative ^ l.imax.negative) == 1 && (r.imin.negative ^ r.imax.negative) == 1)
    {
        // cannot be larger than either l.max or r.max, set the other one to -1
        SignExtendedNumber max = l.imax.value > r.imax.value ? l.imax : r.imax;

        // only negative numbers for minimum
        l.imax.value = -1;
        l.imax.negative = true;
        r.imax.value = -1;
        r.imax.negative = true;

        return IntRange(minAnd(l, r), max);
    }
    else
    {
        // only one interval spans [-1,0]
        if ((l.imin.negative ^ l.imax.negative) == 1)
        {
            swap(l, r); // r spans [-1,0]
        }

        SignExtendedNumber minAndNeg = minAnd(l, IntRange(r.imin, SignExtendedNumber(-1)));
        SignExtendedNumber minAndPos = minAnd(l, IntRange(SignExtendedNumber(0), r.imax));
        SignExtendedNumber maxAndNeg = maxAnd(l, IntRange(r.imin, SignExtendedNumber(-1)));
        SignExtendedNumber maxAndPos = maxAnd(l, IntRange(SignExtendedNumber(0), r.imax));

        SignExtendedNumber min = minAndNeg < minAndPos ? minAndNeg : minAndPos;
        SignExtendedNumber max = maxAndNeg > maxAndPos ? maxAndNeg : maxAndPos;

        return IntRange(min, max);
    }
}

IntRange IntRange::operator|(const IntRange& rhs) const
{
    // unsigned or identical sign bits:
    if ((imin.negative ^ imax.negative) == 0 && (rhs.imin.negative ^ rhs.imax.negative) == 0)
    {
        return IntRange(minOr(*this, rhs), maxOr(*this, rhs));
    }

    IntRange l = IntRange(*this);
    IntRange r = IntRange(rhs);

    // both intervals span [-1,0]
    if ((l.imin.negative ^ l.imax.negative) == 1 && (r.imin.negative ^ r.imax.negative) == 1)
    {
        // cannot be smaller than either l.min or r.min, set the other one to 0
        SignExtendedNumber min = l.imin.value < r.imin.value ? l.imin : r.imin;

        // only negative numbers for minimum
        l.imin.value = 0;
        l.imin.negative = false;
        r.imin.value = 0;
        r.imin.negative = false;

        return IntRange(min, maxOr(l, r));
    }
    else
    {
        // only one interval spans [-1,0]
        if ((imin.negative ^ imax.negative) == 1)
        {
            swap(l, r); // r spans [-1,0]
        }

        SignExtendedNumber minOrNeg = minOr(l, IntRange(r.imin, SignExtendedNumber(-1)));
        SignExtendedNumber minOrPos = minOr(l, IntRange(SignExtendedNumber(0), r.imax));
        SignExtendedNumber maxOrNeg = maxOr(l, IntRange(r.imin, SignExtendedNumber(-1)));
        SignExtendedNumber maxOrPos = maxOr(l, IntRange(SignExtendedNumber(0), r.imax));

        SignExtendedNumber min = minOrNeg < minOrPos ? minOrNeg : minOrPos;
        SignExtendedNumber max = maxOrNeg > maxOrPos ? maxOrNeg : maxOrPos;

        return IntRange(min, max);
    }
}

IntRange IntRange::operator^(const IntRange& rhs) const
{
    return (*this & (~rhs)) | (~(*this) & rhs);
}

IntRange IntRange::operator+(const IntRange& rhs) const
{
    return IntRange(imin + rhs.imin, imax + rhs.imax);
}

IntRange IntRange::operator-(const IntRange& rhs) const
{
    return IntRange(imin - rhs.imax, imax - rhs.imin);
}

IntRange IntRange::operator*(const IntRange& rhs) const
{
    // [a,b] * [c,d] = [min (ac, ad, bc, bd), max (ac, ad, bc, bd)]
    SignExtendedNumber bdy[4];
    bdy[0] = imin * rhs.imin;
    bdy[1] = imin * rhs.imax;
    bdy[2] = imax * rhs.imin;
    bdy[3] = imax * rhs.imax;
    return IntRange::fromNumbers4(bdy);
}

IntRange IntRange::operator/(const IntRange& rhs) const
{
    // Handle divide by 0
    if (rhs.imax.value == 0 && rhs.imin.value == 0)
        return widest();

    IntRange r = IntRange(rhs);

    // Don't treat the whole range as divide by 0 if only one end of a range is 0.
    // Issue 15289
    if (r.imax.value == 0)
    {
        r.imax.value--;
    }
    else if (r.imin.value == 0)
    {
        r.imin.value++;
    }

    if (!imin.negative && !imax.negative && !r.imin.negative && !r.imax.negative)
    {
        return IntRange(imin / r.imax, imax / r.imin);
    }
    else
    {
        // [a,b] / [c,d] = [min (a/c, a/d, b/c, b/d), max (a/c, a/d, b/c, b/d)]
        SignExtendedNumber bdy[4];
        bdy[0] = imin / r.imin;
        bdy[1] = imin / r.imax;
        bdy[2] = imax / r.imin;
        bdy[3] = imax / r.imax;

        return IntRange::fromNumbers4(bdy);
    }
}

IntRange IntRange::operator%(const IntRange& rhs) const
{
    IntRange irNum = *this;
    IntRange irDen = rhs.absNeg();

    /*
     due to the rules of D (C)'s % operator, we need to consider the cases
     separately in different range of signs.

         case 1. [500, 1700] % [7, 23] (numerator is always positive)
             = [0, 22]
         case 2. [-500, 1700] % [7, 23] (numerator can be negative)
             = [-22, 22]
         case 3. [-1700, -500] % [7, 23] (numerator is always negative)
             = [-22, 0]

     the number 22 is the maximum absolute value in the denomator's range. We
     don't care about divide by zero.
     */

    irDen.imin = irDen.imin + SignExtendedNumber(1);
    irDen.imax = -irDen.imin;

    if (!irNum.imin.negative)
    {
        irNum.imin.value = 0;
    }
    else if (irNum.imin < irDen.imin)
    {
        irNum.imin = irDen.imin;
    }

    if (irNum.imax.negative)
    {
        irNum.imax.negative = false;
        irNum.imax.value = 0;
    }
    else if (irNum.imax > irDen.imax)
    {
        irNum.imax = irDen.imax;
    }

    return irNum;
}

IntRange IntRange::operator<<(const IntRange& rhs) const
{
    IntRange r = IntRange(rhs);
    if (r.imin.negative)
    {
        r = IntRange(SignExtendedNumber(0), SignExtendedNumber(64));
    }

    SignExtendedNumber lower = imin << (imin.negative ? r.imax : r.imin);
    SignExtendedNumber upper = imax << (imax.negative ? r.imin : r.imax);

    return IntRange(lower, upper);
}

IntRange IntRange::operator>>(const IntRange& rhs) const
{
    IntRange r = IntRange(rhs);
    if (r.imin.negative)
    {
        r = IntRange(SignExtendedNumber(0), SignExtendedNumber(64));
    }

    SignExtendedNumber lower = imin >> (imin.negative ? r.imin : r.imax);
    SignExtendedNumber upper = imax >> (imax.negative ? r.imax : r.imin);

    return IntRange(lower, upper);
}

SignExtendedNumber IntRange::maxOr(const IntRange& lhs, const IntRange& rhs)
{
    uinteger_t x = 0;
    bool sign = false;
    uinteger_t xorvalue = lhs.imax.value ^ rhs.imax.value;
    uinteger_t andvalue = lhs.imax.value & rhs.imax.value;
    IntRange lhsc = IntRange(lhs);
    IntRange rhsc = IntRange(rhs);

    // Sign bit not part of the .value so we need an extra iteration
    if (lhsc.imax.negative ^ rhsc.imax.negative)
    {
        sign = true;
        if (lhsc.imax.negative)
        {
            if (!lhsc.imin.negative)
            {
                lhsc.imin.value = 0;
            }
            if (!rhsc.imin.negative)
            {
                rhsc.imin.value = 0;
            }
        }
    }
    else if (lhsc.imin.negative & rhsc.imin.negative)
    {
        sign = true;
    }
    else if (lhsc.imax.negative & rhsc.imax.negative)
    {
        return SignExtendedNumber(-1, false);
    }

    for (uinteger_t d = 1ULL << (8 * sizeof(uinteger_t) - 1); d; d >>= 1)
    {
        if (xorvalue & d)
        {
            x |= d;
            if (lhsc.imax.value & d)
            {
                if (~lhsc.imin.value & d)
                {
                    lhsc.imin.value = 0;
                }
            }
            else
            {
                if (~rhsc.imin.value & d)
                {
                    rhsc.imin.value = 0;
                }
            }
        }
        else if (lhsc.imin.value & rhsc.imin.value & d)
        {
            x |= d;
        }
        else if (andvalue & d)
        {
            x |= (d << 1) - 1;
            break;
        }
    }

    return SignExtendedNumber(x, sign);
}

SignExtendedNumber IntRange::minOr(const IntRange& lhs, const IntRange& rhs)
{
    return ~maxAnd(~lhs, ~rhs);
}

SignExtendedNumber IntRange::maxAnd(const IntRange& lhs, const IntRange& rhs)
{
    uinteger_t x = 0;
    bool sign = false;
    IntRange lhsc = IntRange(lhs);
    IntRange rhsc = IntRange(rhs);

    if (lhsc.imax.negative & rhsc.imax.negative)
    {
        sign = true;
    }

    for (uinteger_t d = 1ULL << (8 * sizeof(uinteger_t) - 1); d; d >>= 1)
    {
        if (lhsc.imax.value & rhsc.imax.value & d)
        {
            x |= d;
            if (~lhsc.imin.value & d)
            {
                lhsc.imin.value = 0;
            }
            if (~rhsc.imin.value & d)
            {
                rhsc.imin.value = 0;
            }
        }
        else if (~lhsc.imin.value & d && lhsc.imax.value & d)
        {
            lhsc.imax.value |= d - 1;
        }
        else if (~rhsc.imin.value & d && rhsc.imax.value & d)
        {
            rhsc.imax.value |= d - 1;
        }
    }

    return SignExtendedNumber(x, sign);
}

SignExtendedNumber IntRange::minAnd(const IntRange& lhs, const IntRange& rhs)
{
    return ~maxOr(~lhs, ~rhs);
}

void IntRange::swap(IntRange& a, IntRange& b)
{
    IntRange aux = a;
    a = b;
    b = aux;
}

const IntRange& IntRange::dump(const char* funcName, Expression *e) const
{
    printf("[(%c)%#018llx, (%c)%#018llx] @ %s ::: %s\n",
           imin.negative?'-':'+', (unsigned long long)imin.value,
           imax.negative?'-':'+', (unsigned long long)imax.value,
           funcName, e->toChars());
    return *this;
}
