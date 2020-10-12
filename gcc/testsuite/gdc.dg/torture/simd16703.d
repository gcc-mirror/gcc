// https://issues.dlang.org/show_bug.cgi?id=16703
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

float index(float4 f4, size_t i)
{
    return f4[i];
}

float[4] slice(float4 f4)
{
    return f4[];
}

float slice2(float4 f4, size_t lwr, size_t upr, size_t i)
{
    float[] fa = f4[lwr .. upr];
    return fa[i];
}

void main()
{
    float4 f4 = [1,2,3,4];
    assert(index(f4, 0) == 1);
    assert(index(f4, 1) == 2);
    assert(index(f4, 2) == 3);
    assert(index(f4, 3) == 4);

    float[4] fsa = slice(f4);
    assert(fsa == [1.0f,2,3,4]);

    assert(slice2(f4, 1, 3, 0) == 2);
    assert(slice2(f4, 1, 3, 1) == 3);
}
