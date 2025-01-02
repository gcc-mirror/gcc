/**
 * 80-bit floating point value implementation if the C/D compiler does not support them natively.
 * Copyright (C) 2021-2025 Free Software Foundation, Inc.
 *
 * GCC is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GCC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.
 */

module dmd.root.longdouble;

import core.stdc.config;
import core.stdc.stdint;

extern(C++):
nothrow:
@nogc:
pure:
@trusted:

// Type used by the front-end for compile-time reals
struct longdouble
{
nothrow:
@nogc:
pure:
    extern (D) this(T)(T r)
    {
        this.set(cast(SetType!T)r);
    }

    // No constructor to be able to use this class in a union.
    extern (D) longdouble opAssign(T)(T r)
        if (is (T : longdouble))
    {
        this.realvalue = r.realvalue; 
        return this;
    }

    extern (D) longdouble opAssign(T)(T r)
        if (!is (T : longdouble))
    {
        this.set(cast(SetType!T)r);
        return this;
    }

    // Arithmetic operators.
    extern (D) longdouble opBinary(string op, T)(T r) const
        if ((op == "+" || op == "-" || op == "*" || op == "/" || op == "%")
            && is (T : longdouble))
    {
        static if (op == "+")
            return this.add(r);
        else static if (op == "-")
            return this.sub(r);
        else static if (op == "*")
            return this.mul(r);
        else static if (op == "/")
            return this.div(r);
        else static if (op == "%")
            return this.mod(r);
    }

    extern (D) longdouble opUnary(string op)() const
        if (op == "-")
    {
        return this.neg();
    }

    extern (D) int opCmp(longdouble r) const
    {
        return this.cmp(r);
    }

    extern (D) int opEquals(longdouble r) const
    {
        return this.equals(r);
    }

    extern (D) bool opCast(T : bool)() const
    {
        return this.to_bool();
    }

    extern (D) T opCast(T)() const
    {
        static if (__traits(isUnsigned, T))
            return cast (T) this.to_uint();
        else
            return cast(T) this.to_int();
    }

    void set(int8_t d);
    void set(int16_t d);
    void set(int32_t d);
    void set(int64_t d);
    void set(uint8_t d);
    void set(uint16_t d);
    void set(uint32_t d);
    void set(uint64_t d);
    void set(bool d);

    int64_t to_int() const;
    uint64_t to_uint() const;
    bool to_bool() const;

    longdouble add(const ref longdouble r) const;
    longdouble sub(const ref longdouble r) const;
    longdouble mul(const ref longdouble r) const;
    longdouble div(const ref longdouble r) const;
    longdouble mod(const ref longdouble r) const;
    longdouble neg() const;
    int cmp(const ref longdouble t) const;
    int equals(const ref longdouble t) const;

private:
    // Statically allocate enough space for REAL_VALUE_TYPE.
    enum REALVALUE_SIZE = (2 + (16 + c_long.sizeof) / c_long.sizeof);
    c_long [REALVALUE_SIZE] realvalue;
}

// Pick the corresponding (u)int64_t type for T, as int64_t may be
// a special enum that requires casting to explicitly.
private template SetType(T)
{
    static if (__traits(isIntegral, T) && T.sizeof == 8)
    {
        static if (__traits(isUnsigned, T))
            alias SetType = uint64_t;
        else
            alias SetType = int64_t;
    }
    else
        alias SetType = T;
}
