// https://issues.dlang.org/show_bug.cgi?id=19607
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

int test19607a(const int[4] x)
{
    int sum = 0;
    foreach (i; x) sum += i;
    return sum;
}

void main()
{
    int4 v1 = 1;
    assert(test19607a(v1.array) == 4);
    assert(test19607a(int4(2).array) == 8);
}
