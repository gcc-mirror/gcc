// { dg-additional-options "-mavx2" { target avx2_runtime } }
// { dg-do run { target { avx2_runtime || vect_sizes_32B_16B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void test6a()
{
    // stack occasionally misaligned
    float f = 0;
    long4 v;
    assert((cast(size_t)&v) % 32 == 0);
    v += 1;
}

void test6b()
{
    struct S {long4 v;}
    S s;
    assert((cast(size_t)&s) % 32 == 0);
}

void main()
{
    test6a();
    test6b();
}
