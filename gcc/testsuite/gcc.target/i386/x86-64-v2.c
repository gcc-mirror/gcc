/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mabi=sysv -march=x86-64-v2" } */

/* Verify that the CPU features required by x86-64-v2 are enabled.  */

#ifndef __MMX__
# error __MMX__ not defined
#endif
#ifndef __SSE__
# error __SSE__ not defined
#endif
#ifndef __SSE2__
# error __SSE2__ not defined
#endif
#ifdef __x86_64__
# ifndef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16
#  error __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16 not defined
# endif
#endif
#ifndef __LAHF_SAHF__
# error __LAHF_SAHF__ not defined
#endif
#ifndef __POPCNT__
# error __POPCNT__ not defined
#endif
#ifndef __SSE3__
# error __SSE3__ not defined
#endif
#ifndef __SSE4_1__
# error __SSE4_1__ not defined
#endif
#ifndef __SSE4_2__
# error __SSE4_2__ not defined
#endif
#ifndef __SSSE3__
# error __SSSE3__ not defined
#endif
#ifdef __SSE4A__
# error __SSE4A__ defined
#endif
#ifdef __AVX__
# error __AVX__ defined
#endif
#ifdef __AVX2__
# error __AVX2__ defined
#endif
#ifdef __F16C__
# error __F16C__ defined
#endif
#ifdef __FMA__
# error __FMA__ defined
#endif
#ifdef __LZCNT__
# error __LZCNT__ defined
#endif
#ifdef __MOVBE__
# error __MOVBE__ defined
#endif
#ifdef __XSAVE__
# error __XSAVE__ defined
#endif
#ifdef __XSAVEC__
# error __XSAVEC__ defined
#endif
#ifdef __AVX512F__
# error __AVX512F__ defined
#endif
#ifdef __AVX512BW__
# error __AVX512BW__ defined
#endif
#ifdef __AVX512CD__
# error __AVX512CD__ defined
#endif
#ifdef __AVX512DQ__
# error __AVX512DQ__ defined
#endif
#ifdef __AVX512VL__
# error __AVX512VL__ defined
#endif
#ifdef __AVX512PF__
# error __AVX512PF__ defined
#endif
#ifdef __AVX512VBMI__
# error __AVX512VBMI__ defined
#endif
#ifdef __AVX512IFMA__
# error __AVX512IFMA__ defined
#endif
#ifdef __AVX512VNNIW__
# error __AVX512VNNIW__ defined
#endif
#ifdef __AVX512VBMI2__
# error __AVX512VBMI2__ defined
#endif
#ifdef __AVX5124FMAPS__
# error __AVX5124FMAPS__ defined
#endif
#ifdef __AVX5124BITALG__
# error __AVX5124BITALG__ defined
#endif
#ifdef __AVX5124VPOPCNTDQ__
# error __AVX5124VPOPCNTDQ__ defined
#endif
#ifdef __AVX5124BF16__
# error __AVX5124BF16__ defined
#endif
#ifdef __AVX512VP2INTERSECT__
# error __AVX512VP2INTERSECT__ defined
#endif
#ifdef __AVX512VNNI__
# error __AVX512VNNI__ defined
#endif
#ifdef __FMA4__
# error __FMA4__ defined
#endif
#ifdef __3dNOW__
# error __3dNOW__ defined
#endif
#ifdef __tune_k8__
# error __tune_k8__ defined
#endif
