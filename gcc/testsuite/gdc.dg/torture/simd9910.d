// https://issues.dlang.org/show_bug.cgi?id=9910
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

void main()
{
    float4 f = [1, 1, 1, 1];
    auto works = f + 3;
    auto bug = 3 + f;

    assert (works.array == [4,4,4,4]);
    assert (bug.array == [4,4,4,4]);    // no property 'array' for type 'int'
}
