// https://issues.dlang.org/show_bug.cgi?id=19788
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void test19788()
{
    enum v = __vector(float[4]).init;
    const(float)[] a = v[];
    // { dg-error "'__vector\\\(float\\\[4\\\]\\\)' cannot be sliced with '\\\[\\\]'" "" { target *-*-* } .-1 }
}
