// https://issues.dlang.org/show_bug.cgi?id=21673
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import core.simd;

float4 _mm_move_ss(float4 a, float4 b)
{
    a.ptr[0] = b.array[0];
    return a;
}

void main()
{
    float4 A = [1.0f, 2.0f, 3.0f, 4.0f];
    float4 B = [5.0f, 6.0f, 7.0f, 8.0f];
    float4 R = _mm_move_ss(A, B);
    float[4] correct = [5.0f, 2.0f, 3.0f, 4.0f];
    assert(R.array == correct);
}
