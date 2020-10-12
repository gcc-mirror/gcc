// https://issues.dlang.org/show_bug.cgi?id=19223
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

int test19223a(const int[4] x)
{
    int sum = 0;
    foreach (i; x) sum += i;
    return sum;
}

void main()
{
    int4 v1 = int4.init;
    assert(test19223a(v1.array) == 0);
    assert(test19223a(int4.init.array) == 0);
}
