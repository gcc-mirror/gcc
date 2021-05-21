/* Test whether using target specific options, we can use the x86 builtin
   functions in functions with the appropriate function specific options.  */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=k8" } } */
/* { dg-options "-O2 -march=k8 -mno-sse3 -mfpmath=sse" } */

typedef float     __m128  __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double    __m128d __attribute__ ((__vector_size__ (16), __may_alias__));
typedef int	  __m128w __attribute__ ((__vector_size__ (16), __may_alias__));
typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef char __m128qi __attribute__ ((__vector_size__ (16), __may_alias__));

#ifdef __SSE3__
#error "-msse3 should not be set for this test"
#endif

__m128d sse3_hsubpd (__m128d a, __m128d b) __attribute__((__target__("sse3")));
__m128d generic_hsubpd (__m128d a, __m128d b);

__m128d
sse3_hsubpd (__m128d a, __m128d b)
{
  return __builtin_ia32_hsubpd (a, b);
}

__m128d
generic_hsubpd (__m128d a, __m128d b)
{
  return __builtin_ia32_hsubpd (a, b);			/* { dg-error "needs isa option" } */
}

#ifdef __SSSE3__
#error "-mssse3 should not be set for this test"
#endif

__m128w ssse3_psignd128 (__m128w a, __m128w b) __attribute__((__target__("ssse3")));
__m128w generic_psignd (__m128w ab, __m128w b);

__m128w
ssse3_psignd128 (__m128w a, __m128w b)
{
  return __builtin_ia32_psignd128 (a, b);
}

__m128w
generic_psignd128 (__m128w a, __m128w b)
{
  return __builtin_ia32_psignd128 (a, b);		/* { dg-error "needs isa option" } */
}

#ifdef __SSE4_1__
#error "-msse4.1 should not be set for this test"
#endif

__m128 sse4_1_roundv4sf2 (__m128 a) __attribute__((__target__("sse4.1")));
__m128 generic_roundv4sf2 (__m128 a);

__m128
sse4_1_roundv4sf2  (__m128 a)
{
  return __builtin_ia32_roundps_az (a);
}

__m128
generic_blendvpd  (__m128 a)
{
  return __builtin_ia32_roundps_az (a);		/* { dg-error "needs isa option" } */
}

#ifdef __SSE4_2__
#error "-msse4.2 should not be set for this test"
#endif

__m128qi sse4_2_cmpistrm (__m128qi a, __m128qi b) __attribute__((__target__("sse4.2")));
__m128qi generic_cmpistrm (__m128qi a, __m128qi b);

__m128qi
sse4_2_cmpistrm (__m128qi a, __m128qi b)
{
  return  __builtin_ia32_pcmpistrm128 (a, b, 0);
}

__m128qi
generic_comistrm (__m128qi a, __m128qi b)
{
  return  __builtin_ia32_pcmpistrm128 (a, b, 0);	/* { dg-error "needs isa option" } */
}

#ifdef __SSE4A__
#error "-msse4a should not be set for this test"
#endif

__m128i sse4_2_insertq (__m128i a, __m128i b) __attribute__((__target__("sse4a")));
__m128i generic_insertq (__m128i ab, __m128i b);

__m128i
sse4_2_insertq (__m128i a, __m128i b)
{
  return __builtin_ia32_insertq (a, b);
}

__m128i
generic_insertq (__m128i a, __m128i b)
{
  return __builtin_ia32_insertq (a, b);			/* { dg-error "needs isa option" } */
}

#ifdef __FMA4__
#error "-mfma4 should not be set for this test"
#endif

__m128d fma4_fmaddpd (__m128d a, __m128d b, __m128d c) __attribute__((__target__("fma4")));
__m128d generic_fmaddpd (__m128d a, __m128d b, __m128d c);

__m128d
fma4_fmaddpd  (__m128d a, __m128d b, __m128d c)
{
  return __builtin_ia32_vfmaddpd (a, b, c);
}

__m128d
generic_fmaddpd  (__m128d a, __m128d b, __m128d c)
{
  return __builtin_ia32_vfmaddpd (a, b, c);		/* { dg-error "needs isa option" } */
}

#ifdef __AES__
#error "-maes should not be set for this test"
#endif

__m128i aes_aesimc128 (__m128i a) __attribute__((__target__("aes")));
__m128i generic_aesimc128 (__m128i a);

__m128i
aes_aesimc128 (__m128i a)
{
  return __builtin_ia32_aesimc128 (a);
}

__m128i
generic_aesimc128 (__m128i a)
{
  return __builtin_ia32_aesimc128 (a);			/* { dg-error "needs isa option" } */
}

#ifdef __PCLMUL__
#error "-mpclmul should not be set for this test"
#endif

__m128i pclmul_pclmulqdq128 (__m128i a, __m128i b) __attribute__((__target__("pclmul")));
__m128i generic_pclmulqdq128 (__m128i a, __m128i b);

__m128i
pclmul_pclmulqdq128 (__m128i a, __m128i b)
{
  return __builtin_ia32_pclmulqdq128 (a, b, 5);
}

__m128i
generic_pclmulqdq128 (__m128i a, __m128i b)
{
  return __builtin_ia32_pclmulqdq128 (a, b, 5);		/* { dg-error "needs isa option" } */
}
