/* { dg-do preprocess } */
/* { dg-options "-mno-lsx" } */

#ifdef __loongarch_sx
#error LSX should not be available here
#endif

#ifdef __loongarch_simd_width
#error simd width shuold not be available here
#endif

#pragma GCC push_options
#pragma GCC target("lsx")
#ifndef __loongarch_sx
#error LSX should be available here
#endif
#ifndef __loongarch_simd_width
#error simd width should be available here
#elif __loongarch_simd_width != 128
#error simd width should be 128
#endif
#pragma GCC pop_options

#ifdef __loongarch_sx
#error LSX should become unavailable again
#endif

#ifdef __loongarch_simd_width
#error simd width shuold become unavailable again
#endif
