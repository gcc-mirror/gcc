// { dg-options "-Wno-psabi" }
// { dg-do run { target hw } }
import core.simd;
import core.stdc.string;
import std.stdio;

alias TypeTuple(T...) = T;

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=16087

static assert(void8.sizeof == 8);
static assert(float2.sizeof == 8);
static assert(byte8.sizeof == 8);
static assert(ubyte8.sizeof == 8);
static assert(short4.sizeof == 8);
static assert(ushort4.sizeof == 8);
static assert(int2.sizeof == 8);
static assert(uint2.sizeof == 8);

static assert(void16.alignof == 16);
static assert(double2.alignof == 16);
static assert(float4.alignof == 16);
static assert(byte16.alignof == 16);
static assert(ubyte16.alignof == 16);
static assert(short8.alignof == 16);
static assert(ushort8.alignof == 16);
static assert(int4.alignof == 16);
static assert(uint4.alignof == 16);
static assert(long2.alignof == 16);
static assert(ulong2.alignof == 16);

static assert(void16.sizeof == 16);
static assert(double2.sizeof == 16);
static assert(float4.sizeof == 16);
static assert(byte16.sizeof == 16);
static assert(ubyte16.sizeof == 16);
static assert(short8.sizeof == 16);
static assert(ushort8.sizeof == 16);
static assert(int4.sizeof == 16);
static assert(uint4.sizeof == 16);
static assert(long2.sizeof == 16);
static assert(ulong2.sizeof == 16);

static assert(void32.alignof == 32);
static assert(double4.alignof == 32);
static assert(float8.alignof == 32);
static assert(byte32.alignof == 32);
static assert(ubyte32.alignof == 32);
static assert(short16.alignof == 32);
static assert(ushort16.alignof == 32);
static assert(int8.alignof == 32);
static assert(uint8.alignof == 32);
static assert(long4.alignof == 32);
static assert(ulong4.alignof == 32);

static assert(void32.sizeof == 32);
static assert(double4.sizeof == 32);
static assert(float8.sizeof == 32);
static assert(byte32.sizeof == 32);
static assert(ubyte32.sizeof == 32);
static assert(short16.sizeof == 32);
static assert(ushort16.sizeof == 32);
static assert(int8.sizeof == 32);
static assert(uint8.sizeof == 32);
static assert(long4.sizeof == 32);
static assert(ulong4.sizeof == 32);

/*****************************************/

void test1()
{
    void16 v1 = void,v2 = void;
    byte16 b;
    v2 = b;
    v1 = v2;
    static assert(!__traits(compiles, v1 + v2));
    static assert(!__traits(compiles, v1 - v2));
    static assert(!__traits(compiles, v1 * v2));
    static assert(!__traits(compiles, v1 / v2));
    static assert(!__traits(compiles, v1 % v2));
    static assert(!__traits(compiles, v1 & v2));
    static assert(!__traits(compiles, v1 | v2));
    static assert(!__traits(compiles, v1 ^ v2));
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    static assert(!__traits(compiles, v1 << 1));
    static assert(!__traits(compiles, v1 >> 1));
    static assert(!__traits(compiles, v1 >>> 1));
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    static assert(!__traits(compiles, ~v1));
    static assert(!__traits(compiles, -v1));
    static assert(!__traits(compiles, +v1));
    static assert(!__traits(compiles, !v1));

    static assert(!__traits(compiles, v1 += v2));
    static assert(!__traits(compiles, v1 -= v2));
    static assert(!__traits(compiles, v1 *= v2));
    static assert(!__traits(compiles, v1 /= v2));
    static assert(!__traits(compiles, v1 %= v2));
    static assert(!__traits(compiles, v1 &= v2));
    static assert(!__traits(compiles, v1 |= v2));
    static assert(!__traits(compiles, v1 ^= v2));
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    static assert(!__traits(compiles, v1 <<= 1));
    static assert(!__traits(compiles, v1 >>= 1));
    static assert(!__traits(compiles, v1 >>>= 1));

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2()
{
    byte16 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2b()
{
    ubyte16 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2c()
{
    short8 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;
    v1 = v1 * 3;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2d()
{
    ushort8 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2e()
{
    int4 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2f()
{
    uint4 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2g()
{
    long2 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2h()
{
    ulong2 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    v1 = v2 % v3;
    v1 = v2 & v3;
    v1 = v2 | v3;
    v1 = v2 ^ v3;
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    v1 = v2 << 1;
    v1 = v2 >> 1;
    v1 = v2 >>> 1;
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    v1 = ~v2;
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    v1 %= v2;
    v1 &= v2;
    v1 |= v2;
    v1 ^= v2;
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    v1 <<= 1;
    v1 >>= 1;
    v1 >>>= 1;

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2i()
{
    float4 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    static assert(!__traits(compiles, v1 % v2));
    static assert(!__traits(compiles, v1 & v2));
    static assert(!__traits(compiles, v1 | v2));
    static assert(!__traits(compiles, v1 ^ v2));
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    static assert(!__traits(compiles, v1 << 1));
    static assert(!__traits(compiles, v1 >> 1));
    static assert(!__traits(compiles, v1 >>> 1));
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    static assert(!__traits(compiles, ~v1));
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    static assert(!__traits(compiles, v1 %= v2));
    static assert(!__traits(compiles, v1 &= v2));
    static assert(!__traits(compiles, v1 |= v2));
    static assert(!__traits(compiles, v1 ^= v2));
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    static assert(!__traits(compiles, v1 <<= 1));
    static assert(!__traits(compiles, v1 >>= 1));
    static assert(!__traits(compiles, v1 >>>= 1));

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test2j()
{
    double2 v1, v2 = 1, v3 = 1;
    v1 = v2;
    v1 = v2 + v3;
    v1 = v2 - v3;
    v1 = v2 * v3;
    v1 = v2 / v3;
    static assert(!__traits(compiles, v1 % v2));
    static assert(!__traits(compiles, v1 & v2));
    static assert(!__traits(compiles, v1 | v2));
    static assert(!__traits(compiles, v1 ^ v2));
    static assert(!__traits(compiles, v1 ~ v2));
    static assert(!__traits(compiles, v1 ^^ v2));
    static assert(!__traits(compiles, v1 is v2));
    static assert(!__traits(compiles, v1 !is v2));
    static assert(!__traits(compiles, v1 == v2));
    static assert(!__traits(compiles, v1 != v2));
    static assert(!__traits(compiles, v1 < v2));
    static assert(!__traits(compiles, v1 > v2));
    static assert(!__traits(compiles, v1 <= v2));
    static assert(!__traits(compiles, v1 >= v2));
    static assert(!__traits(compiles, v1 <> v2));
    static assert(!__traits(compiles, v1 !< v2));
    static assert(!__traits(compiles, v1 !> v2));
    static assert(!__traits(compiles, v1 !<> v2));
    static assert(!__traits(compiles, v1 <>= v2));
    static assert(!__traits(compiles, v1 !<= v2));
    static assert(!__traits(compiles, v1 !>= v2));
    static assert(!__traits(compiles, v1 !<>= v2));
    static assert(!__traits(compiles, v1 << 1));
    static assert(!__traits(compiles, v1 >> 1));
    static assert(!__traits(compiles, v1 >>> 1));
    static assert(!__traits(compiles, v1 && v2));
    static assert(!__traits(compiles, v1 || v2));
    static assert(!__traits(compiles, ~v1));
    v1 = -v2;
    v1 = +v2;
    static assert(!__traits(compiles, !v1));

    v1 += v2;
    v1 -= v2;
    v1 *= v2;
    v1 /= v2;
    static assert(!__traits(compiles, v1 %= v2));
    static assert(!__traits(compiles, v1 &= v2));
    static assert(!__traits(compiles, v1 |= v2));
    static assert(!__traits(compiles, v1 ^= v2));
    static assert(!__traits(compiles, v1 ~= v2));
    static assert(!__traits(compiles, v1 ^^= v2));
    static assert(!__traits(compiles, v1 <<= 1));
    static assert(!__traits(compiles, v1 >>= 1));
    static assert(!__traits(compiles, v1 >>>= 1));

    //  A cast from vector to non-vector is allowed only when the target is same size Tsarray.
    static assert(!__traits(compiles, cast(byte)v1));       // 1byte
    static assert(!__traits(compiles, cast(short)v1));      // 2byte
    static assert(!__traits(compiles, cast(int)v1));        // 4byte
    static assert(!__traits(compiles, cast(long)v1));       // 8byte
    static assert(!__traits(compiles, cast(float)v1));      // 4byte
    static assert(!__traits(compiles, cast(double)v1));     // 8byte
    static assert(!__traits(compiles, cast(int[2])v1));     // 8byte Tsarray
    static assert( __traits(compiles, cast(int[4])v1));     // 16byte Tsarray, OK
    static assert( __traits(compiles, cast(long[2])v1));    // 16byte Tsarray, OK
}

/*****************************************/

void test4()
{
    int4 c = 7;
    (cast(int[4])c)[3] = 4;
    (cast(int*)&c)[2] = 4;
    c.array[1] = 4;
    c.ptr[3] = 4;
    assert(c.length == 4);
}

/*****************************************/

void BaseTypeOfVector(T : __vector(T[N]), size_t N)(int i)
{
    assert(is(T == int));
    assert(N == 4);
}


void test7411()
{
    BaseTypeOfVector!(__vector(int[4]))(3);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=7951

float[4] test7951()
{
    float4 v1;
    float4 v2;

    return cast(float[4])(v1+v2);
}

/*****************************************/

void test7951_2()
{
    float[4] v1 = [1,2,3,4];
    float[4] v2 = [1,2,3,4];
    float4 f1, f2, f3;
    f1.array = v1;
    f2.array = v2;
    f3 = f1 + f2;
}

/*****************************************/

immutable ulong2 gulong2 = 0x8000_0000_0000_0000;
immutable uint4 guint4 = 0x8000_0000;
immutable ushort8 gushort8 = 0x8000;
immutable ubyte16 gubyte16 = 0x80;

immutable long2 glong2 = 0x7000_0000_0000_0000;
immutable int4 gint4 = 0x7000_0000;
immutable short8 gshort8 = 0x7000;
immutable byte16 gbyte16 = 0x70;

immutable float4 gfloat4 = 4.0;
immutable double2 gdouble2 = 8.0;

void test7414()
{
    immutable ulong2 lulong2 = 0x8000_0000_0000_0000;
    assert(memcmp(&lulong2, &gulong2, gulong2.sizeof) == 0);

    immutable uint4 luint4 = 0x8000_0000;
    assert(memcmp(&luint4, &guint4, guint4.sizeof) == 0);

    immutable ushort8 lushort8 = 0x8000;
    assert(memcmp(&lushort8, &gushort8, gushort8.sizeof) == 0);

    immutable ubyte16 lubyte16 = 0x80;
    assert(memcmp(&lubyte16, &gubyte16, gubyte16.sizeof) == 0);


    immutable long2 llong2 = 0x7000_0000_0000_0000;
    assert(memcmp(&llong2, &glong2, glong2.sizeof) == 0);

    immutable int4 lint4 = 0x7000_0000;
    assert(memcmp(&lint4, &gint4, gint4.sizeof) == 0);

    immutable short8 lshort8 = 0x7000;
    assert(memcmp(&lshort8, &gshort8, gshort8.sizeof) == 0);

    immutable byte16 lbyte16 = 0x70;
    assert(memcmp(&lbyte16, &gbyte16, gbyte16.sizeof) == 0);


    immutable float4 lfloat4 = 4.0;
    assert(memcmp(&lfloat4, &gfloat4, gfloat4.sizeof) == 0);

    immutable double2 ldouble2 = 8.0;
    assert(memcmp(&ldouble2, &gdouble2, gdouble2.sizeof) == 0);
}

/*****************************************/

void test7413()
{
    byte16 b = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
    assert(b.array[0] == 1);
    assert(b.array[1] == 2);
    assert(b.array[2] == 3);
    assert(b.array[3] == 4);
    assert(b.array[4] == 5);
    assert(b.array[5] == 6);
    assert(b.array[6] == 7);
    assert(b.array[7] == 8);
    assert(b.array[8] == 9);
    assert(b.array[9] == 10);
    assert(b.array[10] == 11);
    assert(b.array[11] == 12);
    assert(b.array[12] == 13);
    assert(b.array[13] == 14);
    assert(b.array[14] == 15);
    assert(b.array[15] == 16);

    ubyte16 ub = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
    assert(ub.array[0] == 1);
    assert(ub.array[1] == 2);
    assert(ub.array[2] == 3);
    assert(ub.array[3] == 4);
    assert(ub.array[4] == 5);
    assert(ub.array[5] == 6);
    assert(ub.array[6] == 7);
    assert(ub.array[7] == 8);
    assert(ub.array[8] == 9);
    assert(ub.array[9] == 10);
    assert(ub.array[10] == 11);
    assert(ub.array[11] == 12);
    assert(ub.array[12] == 13);
    assert(ub.array[13] == 14);
    assert(ub.array[14] == 15);
    assert(ub.array[15] == 16);

    short8 s = [1,2,3,4,5,6,7,8];
    assert(s.array[0] == 1);
    assert(s.array[1] == 2);
    assert(s.array[2] == 3);
    assert(s.array[3] == 4);
    assert(s.array[4] == 5);
    assert(s.array[5] == 6);
    assert(s.array[6] == 7);
    assert(s.array[7] == 8);

    ushort8 us = [1,2,3,4,5,6,7,8];
    assert(us.array[0] == 1);
    assert(us.array[1] == 2);
    assert(us.array[2] == 3);
    assert(us.array[3] == 4);
    assert(us.array[4] == 5);
    assert(us.array[5] == 6);
    assert(us.array[6] == 7);
    assert(us.array[7] == 8);

    int4 i = [1,2,3,4];
    assert(i.array[0] == 1);
    assert(i.array[1] == 2);
    assert(i.array[2] == 3);
    assert(i.array[3] == 4);

    uint4 ui = [1,2,3,4];
    assert(ui.array[0] == 1);
    assert(ui.array[1] == 2);
    assert(ui.array[2] == 3);
    assert(ui.array[3] == 4);

    long2 l = [1,2];
    assert(l.array[0] == 1);
    assert(l.array[1] == 2);

    ulong2 ul = [1,2];
    assert(ul.array[0] == 1);
    assert(ul.array[1] == 2);

    float4 f = [1,2,3,4];
    assert(f.array[0] == 1);
    assert(f.array[1] == 2);
    assert(f.array[2] == 3);
    assert(f.array[3] == 4);

    double2 d = [1,2];
    assert(d.array[0] == 1);
    assert(d.array[1] == 2);
}

/*****************************************/

byte16 b = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
ubyte16 ub = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
short8 s = [1,2,3,4,5,6,7,8];
ushort8 us = [1,2,3,4,5,6,7,8];
int4 i = [1,2,3,4];
uint4 ui = [1,2,3,4];
long2 l = [1,2];
ulong2 ul = [1,2];
float4 f = [1,2,3,4];
double2 d = [1,2];

void test7413_2()
{
    assert(b.array[0] == 1);
    assert(b.array[1] == 2);
    assert(b.array[2] == 3);
    assert(b.array[3] == 4);
    assert(b.array[4] == 5);
    assert(b.array[5] == 6);
    assert(b.array[6] == 7);
    assert(b.array[7] == 8);
    assert(b.array[8] == 9);
    assert(b.array[9] == 10);
    assert(b.array[10] == 11);
    assert(b.array[11] == 12);
    assert(b.array[12] == 13);
    assert(b.array[13] == 14);
    assert(b.array[14] == 15);
    assert(b.array[15] == 16);

    assert(ub.array[0] == 1);
    assert(ub.array[1] == 2);
    assert(ub.array[2] == 3);
    assert(ub.array[3] == 4);
    assert(ub.array[4] == 5);
    assert(ub.array[5] == 6);
    assert(ub.array[6] == 7);
    assert(ub.array[7] == 8);
    assert(ub.array[8] == 9);
    assert(ub.array[9] == 10);
    assert(ub.array[10] == 11);
    assert(ub.array[11] == 12);
    assert(ub.array[12] == 13);
    assert(ub.array[13] == 14);
    assert(ub.array[14] == 15);
    assert(ub.array[15] == 16);

    assert(s.array[0] == 1);
    assert(s.array[1] == 2);
    assert(s.array[2] == 3);
    assert(s.array[3] == 4);
    assert(s.array[4] == 5);
    assert(s.array[5] == 6);
    assert(s.array[6] == 7);
    assert(s.array[7] == 8);

    assert(us.array[0] == 1);
    assert(us.array[1] == 2);
    assert(us.array[2] == 3);
    assert(us.array[3] == 4);
    assert(us.array[4] == 5);
    assert(us.array[5] == 6);
    assert(us.array[6] == 7);
    assert(us.array[7] == 8);

    assert(i.array[0] == 1);
    assert(i.array[1] == 2);
    assert(i.array[2] == 3);
    assert(i.array[3] == 4);

    assert(ui.array[0] == 1);
    assert(ui.array[1] == 2);
    assert(ui.array[2] == 3);
    assert(ui.array[3] == 4);

    assert(l.array[0] == 1);
    assert(l.array[1] == 2);

    assert(ul.array[0] == 1);
    assert(ul.array[1] == 2);

    assert(f.array[0] == 1);
    assert(f.array[1] == 2);
    assert(f.array[2] == 3);
    assert(f.array[3] == 4);

    assert(d.array[0] == 1);
    assert(d.array[1] == 2);
}

/*****************************************/

float bug8060(float x) {
    int i = *cast(int*)&x;
    ++i;
    return *cast(float*)&i;
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=9200

void bar9200(double[2] a)
{
    assert(a[0] == 1);
    assert(a[1] == 2);
}

double2 * v9200(double2* a)
{
    return a;
}

void test9200()
{
    double2 a = [1, 2];

    *v9200(&a) = a;

    bar9200(a.array);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=9304
// https://issues.dlang.org/show_bug.cgi?id=9322

float4 foo9304(float4 a)
{
    return -a;
}


void test9304()
{
    auto a = foo9304([0, 1, 2, 3]);
    //writeln(a.array);
    assert(a.array == [0,-1,-2,-3]);
}

/*****************************************/

void test9910()
{
    float4 f = [1, 1, 1, 1];
    auto works = f + 3;
    auto bug = 3 + f;

    assert (works.array == [4,4,4,4]);
    assert (bug.array == [4,4,4,4]);    // no property 'array' for type 'int'
}

/*****************************************/

bool normalize(double[] range, double sum = 1)
{
    double s = 0;
    const length = range.length;
    foreach (e; range)
    {
        s += e;
    }
    if (s == 0)
    {
        return false;
    }
    return true;
}

void test12852()
{
    double[3] range = [0.0, 0.0, 0.0];
    assert(normalize(range[]) == false);
    range[1] = 3.0;
    assert(normalize(range[]) == true);
}

/*****************************************/

void test9449()
{
    ubyte16[1] table;
}

/*****************************************/

void test9449_2()
{
    float[4][2] m = [[2.0, 1, 3, 4], [5.0, 6, 7, 8]];   // segfault

    assert(m[0][0] == 2.0);
    assert(m[0][1] == 1);
    assert(m[0][2] == 3);
    assert(m[0][3] == 4);

    assert(m[1][0] == 5.0);
    assert(m[1][1] == 6);
    assert(m[1][2] == 7);
    assert(m[1][3] == 8);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=13841

void test13841()
{
    alias Vector16s = TypeTuple!(
        void16,  byte16,  short8,  int4,  long2,
                ubyte16, ushort8, uint4, ulong2, float4, double2);
    foreach (V1; Vector16s)
    {
        foreach (V2; Vector16s)
        {
            V1 v1 = void;
            V2 v2 = void;
            static if (is(V1 == V2))
            {
                static assert( is(typeof(true ? v1 : v2) == V1));
            }
            else
            {
                static assert(!is(typeof(true ? v1 : v2)));
            }
        }
    }
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=12776

void test12776()
{
    alias Vector16s = TypeTuple!(
        void16,  byte16,  short8,  int4,  long2,
                ubyte16, ushort8, uint4, ulong2, float4, double2);
    foreach (V; Vector16s)
    {
        static assert(is(typeof(                   V .init) ==                    V ));
        static assert(is(typeof(             const(V).init) ==              const(V)));
        static assert(is(typeof(       inout(      V).init) ==        inout(      V)));
        static assert(is(typeof(       inout(const V).init) ==        inout(const V)));
        static assert(is(typeof(shared(            V).init) == shared(            V)));
        static assert(is(typeof(shared(      const V).init) == shared(      const V)));
        static assert(is(typeof(shared(inout       V).init) == shared(inout       V)));
        static assert(is(typeof(shared(inout const V).init) == shared(inout const V)));
        static assert(is(typeof(         immutable(V).init) ==          immutable(V)));
    }
}

/*****************************************/

void foo13988(double[] arr)
{
    static ulong repr(double d) { return *cast(ulong*)&d; }
    foreach (x; arr)
        assert(repr(arr[0]) == *cast(ulong*)&(arr[0]));
}


void test13988()
{
    double[] arr = [3.0];
    foo13988(arr);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=15123

void test15123()
{
    alias Vector16s = TypeTuple!(
        void16,  byte16,  short8,  int4,  long2,
                ubyte16, ushort8, uint4, ulong2, float4, double2);
    foreach (V; Vector16s)
    {
        auto x = V.init;
    }
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=15144

void test15144()
{
        enum      ubyte16 csXMM1 = ['a','b','c',0,0,0,0,0];
        __gshared ubyte16 csXMM2 = ['a','b','c',0,0,0,0,0];
        immutable ubyte16 csXMM3 = ['a','b','c',0,0,0,0,0];
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=13927

void test13927(ulong2 a)
{
    ulong2 b = [long.min, long.min];
    auto tmp = a - b;
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=16488

void foo_byte16(byte t, byte s)
{
    byte16 f = s;
    auto p = cast(byte*)&f;
    foreach (i; 0 .. 16)
        assert(p[i] == s);
}

void foo_ubyte16(ubyte t, ubyte s)
{
    ubyte16 f = s;
    auto p = cast(ubyte*)&f;
    foreach (i; 0 .. 16)
        assert(p[i] == s);
}


void foo_short8(short t, short s)
{
    short8 f = s;
    auto p = cast(short*)&f;
    foreach (i; 0 .. 8)
        assert(p[i] == s);
}

void foo_ushort8(ushort t, ushort s)
{
    ushort8 f = s;
    auto p = cast(ushort*)&f;
    foreach (i; 0 .. 8)
        assert(p[i] == s);
}


void foo_int4(int t, int s)
{
    int4 f = s;
    auto p = cast(int*)&f;
    foreach (i; 0 .. 4)
        assert(p[i] == s);
}

void foo_uint4(uint t, uint s, uint u)
{
    uint4 f = s;
    auto p = cast(uint*)&f;
    foreach (i; 0 .. 4)
        assert(p[i] == s);
}


void foo_long2(long t, long s, long u)
{
    long2 f = s;
    auto p = cast(long*)&f;
    foreach (i; 0 .. 2)
        assert(p[i] == s);
}

void foo_ulong2(ulong t, ulong s)
{
    ulong2 f = s;
    auto p = cast(ulong*)&f;
    foreach (i; 0 .. 2)
        assert(p[i] == s);
}

void foo_float4(float t, float s)
{
    float4 f = s;
    auto p = cast(float*)&f;
    foreach (i; 0 .. 4)
        assert(p[i] == s);
}

void foo_double2(double t, double s, double u)
{
    double2 f = s;
    auto p = cast(double*)&f;
    foreach (i; 0 .. 2)
        assert(p[i] == s);
}


void test16448()
{
    foo_byte16(5, -10);
    foo_ubyte16(5, 11);

    foo_short8(5, -6);
    foo_short8(5, 7);

    foo_int4(5, -6);
    foo_uint4(5, 0x12345678, 22);

    foo_long2(5, -6, 1);
    foo_ulong2(5, 0x12345678_87654321L);

    foo_float4(5, -6);
    foo_double2(5, -6, 2);
}

/*****************************************/

void foo_byte32(byte t, byte s)
{
    byte32 f = s;
    auto p = cast(byte*)&f;
    foreach (i; 0 .. 32)
        assert(p[i] == s);
}

void foo_ubyte32(ubyte t, ubyte s)
{
    ubyte32 f = s;
    auto p = cast(ubyte*)&f;
    foreach (i; 0 .. 32)
        assert(p[i] == s);
}

void foo_short16(short t, short s)
{
    short16 f = s;
    auto p = cast(short*)&f;
    foreach (i; 0 .. 16)
        assert(p[i] == s);
}

void foo_ushort16(ushort t, ushort s)
{
    ushort16 f = s;
    auto p = cast(ushort*)&f;
    foreach (i; 0 .. 16)
        assert(p[i] == s);
}

void foo_int8(int t, int s)
{
    int8 f = s;
    auto p = cast(int*)&f;
    foreach (i; 0 .. 8)
        assert(p[i] == s);
}

void foo_uint8(uint t, uint s, uint u)
{
    uint8 f = s;
    auto p = cast(uint*)&f;
    foreach (i; 0 .. 8)
        assert(p[i] == s);
}

void foo_long4(long t, long s, long u)
{
    long4 f = s;
    auto p = cast(long*)&f;
    foreach (i; 0 .. 4)
        assert(p[i] == s);
}

void foo_ulong4(ulong t, ulong s)
{
    ulong4 f = s;
    auto p = cast(ulong*)&f;
    foreach (i; 0 .. 4)
        assert(p[i] == s);
}

void foo_float8(float t, float s)
{
    float8 f = s;
    auto p = cast(float*)&f;
    foreach (i; 0 .. 8)
        assert(p[i] == s);
}

void foo_double4(double t, double s, double u)
{
    double4 f = s;
    auto p = cast(double*)&f;
    foreach (i; 0 .. 4)
        assert(p[i] == s);
}

void test16448_32()
{
    import core.cpuid;
    if (!core.cpuid.avx)
        return;

    foo_byte32(5, -10);
    foo_ubyte32(5, 11);

    foo_short16(5, -6);
    foo_short16(5, 7);

    foo_int8(5, -6);
    foo_uint8(5, 0x12345678, 22);

    foo_long4(5, -6, 1);
    foo_ulong4(5, 0x12345678_87654321L);

    foo_float8(5, -6);
    foo_double4(5, -6, 2);
}


/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=16703

float index(float4 f4, size_t i)
{
    return f4[i];
    //return (*cast(float[4]*)&f4)[2];
}

float[4] slice(float4 f4)
{
    return f4[];
}

float slice2(float4 f4, size_t lwr, size_t upr, size_t i)
{
    float[] fa = f4[lwr .. upr];
    return fa[i];
}

void test16703()
{
    float4 f4 = [1,2,3,4];
    assert(index(f4, 0) == 1);
    assert(index(f4, 1) == 2);
    assert(index(f4, 2) == 3);
    assert(index(f4, 3) == 4);

    float[4] fsa = slice(f4);
    assert(fsa == [1.0f,2,3,4]);

    assert(slice2(f4, 1, 3, 0) == 2);
    assert(slice2(f4, 1, 3, 1) == 3);
}

/*****************************************/

struct Sunsto
{
  align (1): // make sure f4 is misaligned
    byte b;
    union
    {
        float4 f4;
        ubyte[16] a;
    }
}

ubyte[16] foounsto()
{
    float4 vf = 6;
    Sunsto s;
    s.f4 = vf * 2;
    vf = s.f4;

    return s.a;
}

void testOPvecunsto()
{
    auto a = foounsto();
    version (LittleEndian)
        assert(a == [0, 0, 64, 65, 0, 0, 64, 65, 0, 0, 64, 65, 0, 0, 64, 65]);
    version (BigEndian)
        assert(a == [65, 64, 0, 0, 65, 64, 0, 0, 65, 64, 0, 0, 65, 64, 0, 0]);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=10447

void test10447()
{
    immutable __vector(double[2]) a = [1.0, 2.0];
    __vector(double[2]) r;
    r += a;
    r = r * a;
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=17237

struct S17237
{
    bool a;
    struct
    {
        bool b;
        int8 c;
    }
}

static assert(S17237.a.offsetof == 0);
static assert(S17237.b.offsetof == 32);
static assert(S17237.c.offsetof == 64);

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=16697

static assert(!is(float == __vector));
static assert(!is(float[1] == __vector));
static assert(!is(float[4] == __vector));
static assert( is(__vector(float[4]) == __vector));
static assert(!is(__vector(float[3]) == __vector));
static assert(!is(__vector(float[5]) == __vector));
static assert( is(__vector(float[4]) X == __vector) && is(X == float[4]));
static assert( is(__vector(byte[16]) X == __vector) && is(X == byte[16]));

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=17720

void test17720()
{
    alias Vector16s = TypeTuple!(
        void16,  byte16,  short8,  int4,  long2,
                ubyte16, ushort8, uint4, ulong2, float4, double2);
    alias Vector32s = TypeTuple!(
        void32,  byte32,  short16,  int8,  long4,
                ubyte32, ushort16, uint8, ulong4, float8, double4);

    // OK: __vector(T) -> __vector(void[]) of same size.
    // NG: __vector(T) -> __vector(void[]) of different size.
    // NG: explicit cast __vector(T) -> __vector(void[]) of different size.
    foreach (V; Vector16s)
    {
        static assert( __traits(compiles, { void16 v = V.init; }));
        static assert(!__traits(compiles, { void32 v = V.init; }));
        static assert(!__traits(compiles, { void32 v = cast(void32)V.init; }));
    }
    foreach (V; Vector32s)
    {
        static assert( __traits(compiles, { void32 v = V.init; }));
        static assert(!__traits(compiles, { void16 v = V.init; }));
        static assert(!__traits(compiles, { void16 v = cast(void16)V.init; }));
    }

    // NG: __vector(T) -> __vector(T) of same size.
    // OK: explicit cast __vector(T) -> __vector(T) of same size.
    // NG: __vector(T) -> __vector(T) of different size.
    // NG: explicit cast __vector(T) -> __vector(T) of different size.
    foreach (V; Vector16s)
    {
        static if (is(V == double2))
        {
            static assert(!__traits(compiles, { long2 v = V.init; }));
            static assert( __traits(compiles, { long2 v = cast(long2)V.init; }));
        }
        else
        {
            static assert(!__traits(compiles, { double2 v = V.init; }));
            static assert( __traits(compiles, { double2 v = cast(double2)V.init; }));
        }
        static assert(!__traits(compiles, { double4 v = V.init; }));
        static assert(!__traits(compiles, { double4 v = cast(double4)V.init; }));
    }
    foreach (V; Vector32s)
    {
        static if (is(V == double4))
        {
            static assert(!__traits(compiles, { long4 v = V.init; }));
            static assert( __traits(compiles, { long4 v = cast(long4)V.init; }));
        }
        else
        {
            static assert(!__traits(compiles, { double4 v = V.init; }));
            static assert( __traits(compiles, { double4 v = cast(double4)V.init; }));
        }
        static assert(!__traits(compiles, { double2 v = V.init; }));
        static assert(!__traits(compiles, { double2 v = cast(double2)V.init; }));
    }
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=17695

void test17695(__vector(ubyte[16]) a)
{
    auto b = -a;
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19223

int test19223a(const int[4] x)
{
    int sum = 0;
    foreach (i; x) sum += i;
    return sum;
}

void test19223()
{
    int4 v1 = int4.init;
    assert(test19223a(v1.array) == 0);
    assert(test19223a(int4.init.array) == 0);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19224

float test19224(const float[4] val)
{
    float sum = 0;
    foreach (x; val) sum += x;
    return sum;
}

enum x19224 = test19224(float4.init.array);
static assert(x19224 is float.nan);

enum y19224 = test19224(float4(1).array);
static assert(y19224 == 4);

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19607

int test19607a(const int[4] x)
{
    int sum = 0;
    foreach (i; x) sum += i;
    return sum;
}

void test19607()
{
    int4 v1 = 1;
    assert(test19607a(v1.array) == 4);
    assert(test19607a(int4(2).array) == 8);
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19627

enum int[4] fail19627 = cast(int[4])int4(0);

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19628

enum ice19628a = int4.init[0];
enum ice19628b = int4.init.array[0];
enum ice19628c = (cast(int[4])int4.init.array)[0];
enum ice19628d = (cast(int[4])int4.init)[0];

enum int4 v19628a = int4.init;
enum idx19628a = v19628a[0];
static assert(idx19628a == 0);

enum int[4] v19628b = int4.init.array;
enum idx19628b = v19628b[0];
static assert(idx19628b == 0);

enum int[4] v19628c = cast(int[4])int4.init.array;
enum idx19628c = v19628c[0];
static assert(idx19628c == 0);

enum int[4] v19628d = cast(int[4])int4.init;
enum idx19628d = v19628d[0];
static assert(idx19628d == 0);

immutable int4 v19628e = int4.init;
immutable idx19628e = v19628e[0];
static assert(idx19628e == 0);

immutable int[4] v19628f = int4.init.array;
immutable idx19628f = v19628f[0];
static assert(idx19628f == 0);

immutable int[4] v19628g = cast(int[4])int4.init.array;
immutable idx19628g = v19628g[0];
static assert(idx19628g == 0);

immutable idx19628h = v19628h[0];
immutable int[4] v19628h = cast(int[4])int4.init;
static assert(idx19628h == 0);

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19629

enum fail19629a = int4(0)[0];
enum fail19629b = int4(0).array[0];
enum fail19629c = (cast(int[4])int4(0).array)[0];
enum fail19628d = (cast(int[4])int4(0))[0];

enum int4 v19629a = int4(0);
enum idx19629a = v19629a[0];
static assert(idx19629a == 0);

enum int[4] v19629b = int4(0).array;
enum idx19629b = v19629b[0];
static assert(idx19629b == 0);

enum int[4] v19629c = cast(int[4])int4(0).array;
enum idx19629c = v19629c[0];
static assert(idx19629c == 0);

enum int[4] v19629d = cast(int[4])int4(0);
enum idx19629d = v19629d[0];
static assert(idx19629d == 0);

immutable int4 v19629e = int4(0);
immutable idx19629e = v19629e[0];
static assert(idx19629e == 0);

immutable int[4] v19629f = int4(0).array;
immutable idx19629f = v19629f[0];
static assert(idx19629f == 0);

immutable int[4] v19629g = cast(int[4])int4(0).array;
immutable idx19629g = v19629g[0];
static assert(idx19629g == 0);

immutable int[4] v19629h = cast(int[4])int4(0);
immutable idx19629h = v19629h[0];
static assert(idx19629h == 0);

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19630

enum fail19630a = int4.init[1..2];
enum fail19630b = int4.init.array[1..2];
enum fail19630c = (cast(int[4])int4.init.array)[1..2];
enum fail19630d = (cast(int[4])int4.init)[1..2];
enum fail19630e = int4(0)[1..2];
enum fail19630f = int4(0).array[1..2];
enum fail19630g = (cast(int[4])int4(0).array)[1..2];
enum fail19630h = (cast(int[4])int4(0))[1..2];

enum int4 v19630a = int4.init;
enum slice19630a = v19630a[1..2];
static assert(slice19630a == [0]);

enum int[4] v19630b = int4.init.array;
enum slice19630b = v19630b[1..2];
static assert(slice19630b == [0]);

enum int[4] v19630c = cast(int[4])int4.init.array;
enum slice19630c = v19630c[1..2];
static assert(slice19630c == [0]);

enum int[4] v19630d = cast(int[4])int4.init;
enum slice19630d = v19630d[1..2];
static assert(slice19630d == [0]);

enum int4 v19630e = int4(0);
enum slice19630e = v19630e[1..2];
static assert(slice19630e == [0]);

enum int[4] v19630f = int4(0).array;
enum slice19630f = v19630f[1..2];
static assert(slice19630f == [0]);

enum int[4] v19630g = cast(int[4])int4(0).array;
enum slice19630g = v19630g[1..2];
static assert(slice19630g == [0]);

enum int[4] v19630h = cast(int[4])int4(0);
enum slice19630h = v19630h[1..2];
static assert(slice19630h == [0]);

immutable int4 v19630i = int4.init;
immutable slice19630i = v19630i[1..2];
static assert(slice19630i == [0]);

immutable int[4] v19630j = int4.init.array;
immutable slice19630j = v19630j[1..2];
static assert(slice19630j == [0]);

immutable int[4] v19630k = cast(int[4])int4.init.array;
immutable slice19630k = v19630k[1..2];
static assert(slice19630k == [0]);

immutable int[4] v19630l = cast(int[4])int4.init;
immutable slice19630l = v19630l[1..2];
static assert(slice19630l == [0]);

immutable int4 v19630m = int4(0);
immutable slice19630m = v19630m[1..2];
static assert(slice19630m == [0]);

immutable int[4] v19630n = int4(0).array;
immutable slice19630n = v19630n[1..2];
static assert(slice19630n == [0]);

immutable int[4] v19630o = cast(int[4])int4(0).array;
immutable slice19630o = v19630o[1..2];
static assert(slice19630o == [0]);

immutable int[4] v19630p = cast(int[4])int4(0);
immutable slice19630p = v19630p[1..2];
static assert(slice19630p == [0]);

/*****************************************/

int main()
{
    test1();
    test2();
    test2b();
    test2c();
    test2d();
    test2e();
    test2f();
    test2g();
    test2h();
    test2i();
    test2j();

    test4();
    test7411();

    test7951();
    test7951_2();
    test7414();
    test7413();
    test7413_2();
    test9200();
    test9304();
    test9910();
    test12852();
    test9449();
    test9449_2();
    test13988();
    test16448();
    test16448_32();
    test16703();
    testOPvecunsto();
    test10447();

    test19223();
    test19607();

    return 0;
}
