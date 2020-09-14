// https://issues.dlang.org/show_bug.cgi?id=16488
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

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

void main()
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
