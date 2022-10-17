// { dg-additional-options "-mavx2" { target avx2_runtime } }
// { dg-do run { target { avx2_runtime || vect_sizes_32B_16B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

auto test20052()
{
    struct S { long4 v; }
    S s;
    return s;
}

void main()
{
    test20052();
}

