/* { dg-do compile } */
/* { dg-options "-O2 -march=native -mno-avx512bmm" } */

#if defined(__ZNVER6__) && defined(__AVX512BMM__)
# error AVX512BMM should be disabled for native detection of target znver6 with -mno-avx512bmm.
#endif
