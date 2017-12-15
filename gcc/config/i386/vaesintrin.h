#ifndef __VAESINTRIN_H_INCLUDED
#define __VAESINTRIN_H_INCLUDED

#ifndef __VAES__
#pragma GCC push_options
#pragma GCC target("vaes")
#define __DISABLE_VAES__
#endif /* __VAES__ */

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_aesdec_epi128 (__m256i __A, __m256i __B)
{
  return (__m256i)__builtin_ia32_vaesdec_v32qi ((__v32qi) __A, (__v32qi) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_aesdeclast_epi128 (__m256i __A, __m256i __B)
{
  return (__m256i)__builtin_ia32_vaesdeclast_v32qi ((__v32qi) __A,
								(__v32qi) __B);
}

#ifdef __DISABLE_VAES__
#undef __DISABLE_VAES__
#pragma GCC pop_options
#endif /* __DISABLE_VAES__ */


#if !defined(__VAES__) || !defined(__AVX512F)
#pragma GCC push_options
#pragma GCC target("vaes,avx512f")
#define __DISABLE_VAESF__
#endif /* __VAES__ */


extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_aesdec_epi128 (__m512i __A, __m512i __B)
{
  return (__m512i)__builtin_ia32_vaesdec_v64qi ((__v64qi) __A, (__v64qi) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_aesdeclast_epi128 (__m512i __A, __m512i __B)
{
  return (__m512i)__builtin_ia32_vaesdeclast_v64qi ((__v64qi) __A,
						    (__v64qi) __B);
}

#ifdef __DISABLE_VAESF__
#undef __DISABLE_VAESF__
#pragma GCC pop_options
#endif /* __DISABLE_VAES__ */

#if !defined(__VAES__) || !defined(__AVX512VL)
#pragma GCC push_options
#pragma GCC target("vaes,avx512vl")
#define __DISABLE_VAESVL__
#endif /* __VAES__ */

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesdec_epi128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_vaesdec_v16qi ((__v16qi) __A, (__v16qi) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesdeclast_epi128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_vaesdeclast_v16qi ((__v16qi) __A,
						    (__v16qi) __B);
}

#ifdef __DISABLE_VAESVL__
#undef __DISABLE_VAESVL__
#pragma GCC pop_options
#endif /* __DISABLE_VAES__ */
#endif /* __VAESINTRIN_H_INCLUDED */
