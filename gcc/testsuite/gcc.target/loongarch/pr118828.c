/* { dg-do preprocess } */
/* { dg-options "-mno-lasx" } */

#ifdef __loongarch_asx
#error LASX should not be available here
#endif

#ifdef __loongarch_simd_width
#if __loongarch_simd_width == 256
#error simd width shuold not be 256
#endif
#endif

#pragma GCC push_options
#pragma GCC target("lasx")
#ifndef __loongarch_asx
#error LASX should be available here
#endif
#ifndef __loongarch_simd_width
#error simd width should be available here
#elif __loongarch_simd_width != 256
#error simd width should be 256
#endif
#pragma GCC pop_options

#ifdef __loongarch_asx
#error LASX should become unavailable again
#endif

#ifdef __loongarch_simd_width
#if __loongarch_simd_width == 256
#error simd width shuold not be 256 again
#endif
#endif
