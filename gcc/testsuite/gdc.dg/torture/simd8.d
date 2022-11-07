// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
import gcc.simd;

alias __m128 = __vector(float[4]);

__m128 _mm_setr_ps (float e3, float e2, float e1, float e0) pure @trusted
{
    float[4] result = [e3, e2, e1, e0];
    return loadUnaligned!(__m128)(cast(__m128*)result.ptr);
}

__m128 _mm_movehdup_ps (__m128 a) pure @trusted
{
    a.ptr[0] = a.array[1];
    a.ptr[2] = a.array[3];
    return a;
}

void main()
{
    __m128 A = _mm_movehdup_ps(_mm_setr_ps(1, 2, 3, 4));
    float[4] correct = [2.0f, 2, 4, 4 ];
    assert(A.array == correct);
}
