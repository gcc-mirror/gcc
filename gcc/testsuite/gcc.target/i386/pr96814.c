/* { dg-do run } */
/* { dg-options "-mavx512vl -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */

typedef unsigned char __attribute__ ((__vector_size__ (32))) V;

void
test (void)
{
  V x = ((V){8} > 0) == 0;
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != (i ? 0xff : 0)) __builtin_abort();
}

#define DO_TEST test
#define AVX512VL
#define AVX512BW
#include "avx512-check.h"
