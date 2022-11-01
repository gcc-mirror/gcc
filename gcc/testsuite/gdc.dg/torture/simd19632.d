// https://issues.dlang.org/show_bug.cgi?id=19632
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void main()
{
    int4 v = [1, 2, 3, 4];
    int sum = 0;
    foreach (ref e; v)
        sum += (e *= 2);
    assert(v.array[] == [2, 4, 6, 8]);
    assert(sum == 20);
}
