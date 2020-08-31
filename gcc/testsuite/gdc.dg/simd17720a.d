// https://issues.dlang.org/show_bug.cgi?id=17720
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

alias TypeTuple(T...) = T;

void test17720()
{
    alias Vector16s = TypeTuple!(
        void16,  byte16,  short8,  int4,  long2,
                ubyte16, ushort8, uint4, ulong2, float4, double2);

    // OK: __vector(T) -> __vector(void[]) of same size.
    // NG: __vector(T) -> __vector(void[]) of different size.
    // NG: explicit cast __vector(T) -> __vector(void[]) of different size.
    foreach (V; Vector16s)
    {
        static assert( __traits(compiles, { void16 v = V.init; }));
        static assert(!__traits(compiles, { void32 v = V.init; }));
        static assert(!__traits(compiles, { void32 v = cast(void32)V.init; }));
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
}
