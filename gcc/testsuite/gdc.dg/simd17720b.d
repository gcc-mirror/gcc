// https://issues.dlang.org/show_bug.cgi?id=17720
// { dg-additional-options "-mavx2" { target avx2_runtime } }
// { dg-do compile { target { avx2_runtime || vect_sizes_32B_16B } } }
import core.simd;

alias TypeTuple(T...) = T;

void test17720()
{
    alias Vector32s = TypeTuple!(
        void32,  byte32,  short16,  int8,  long4,
                ubyte32, ushort16, uint8, ulong4, float8, double4);

    // OK: __vector(T) -> __vector(void[]) of same size.
    // NG: __vector(T) -> __vector(void[]) of different size.
    // NG: explicit cast __vector(T) -> __vector(void[]) of different size.
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
