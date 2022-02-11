// https://issues.dlang.org/show_bug.cgi?id=17344
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void main()
{
    __vector(int[4]) vec1 = 2, vec2 = vec1++;
    assert(cast(int[4])vec1 == [3, 3, 3, 3]);
    assert(cast(int[4])vec2 == [2, 2, 2, 2]);
}
