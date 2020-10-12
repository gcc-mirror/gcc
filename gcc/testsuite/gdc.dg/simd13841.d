// https://issues.dlang.org/show_bug.cgi?id=13841
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

alias TypeTuple(T...) = T;

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
