/* { dg-do preprocess { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-mno-avx" } */

#ifdef __AVX__
#error "__AVX__ should not be defined #1"
#endif

#pragma GCC target("avx")
#ifndef __AVX__
#error "__AVX__ should be defined #1"
#endif

#pragma GCC reset_options
#ifdef __AVX__
#error "__AVX__ should not be defined #2"
#endif

#pragma GCC push_options
#pragma GCC target("avx")
#ifndef __AVX__
#error "__AVX__ should be defined #2"
#endif

#pragma GCC pop_options
#ifdef __AVX__
#error "__AVX__ should not be defined #3"
#endif
