// https://issues.dlang.org/show_bug.cgi?id=16488
// { dg-additional-options "-mavx2" { target avx2_runtime } }
// { dg-do run { target { avx2_runtime || vect_sizes_32B_16B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

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

void main()
{
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
