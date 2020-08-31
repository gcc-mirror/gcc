// https://issues.dlang.org/show_bug.cgi?id=9304
// https://issues.dlang.org/show_bug.cgi?id=9322
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

float4 foo9304(float4 a)
{
    return -a;
}

void main()
{
    auto a = foo9304([0, 1, 2, 3]);
    assert(a.array == [0,-1,-2,-3]);
}
