// https://issues.dlang.org/show_bug.cgi?id=12776
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
import core.simd;

alias TypeTuple(T...) = T;

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
