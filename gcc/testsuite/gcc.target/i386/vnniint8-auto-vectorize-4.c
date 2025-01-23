/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#define N 512

#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#define AVX512F_LEN 512

#define TEST test_512

#ifndef CHECK
#define CHECK "avx10-check.h"
#endif

#include "vnniint8-auto-vectorize-2.c"
