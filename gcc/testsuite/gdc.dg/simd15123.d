// https://issues.dlang.org/show_bug.cgi?id=15123
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

alias TypeTuple(T...) = T;

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
