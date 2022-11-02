// https://issues.dlang.org/show_bug.cgi?id=20041
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

immutable(float4) test20041()
{
    float4 raw = 2.0f;
    raw.array[0] = 1;
    return cast(immutable)raw;
}

void main()
{
    static immutable float4 v = test20041();

    assert(v.array[0] == 1);
    assert(v.array[1] == 2);
    assert(v.array[2] == 2);
    assert(v.array[3] == 2);
}
