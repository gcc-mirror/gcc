typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
extern __m128 _mm_sub_ps (__m128 __A, __m128 __B);
extern __m128 _mm_mul_ps (__m128 __A, __m128 __B);
__m128
vq_nbest(const __m128 *codebook, __m128 d, __m128 in)
{
  return _mm_sub_ps(d, _mm_mul_ps(in, *codebook++));
}
