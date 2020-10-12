// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

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
