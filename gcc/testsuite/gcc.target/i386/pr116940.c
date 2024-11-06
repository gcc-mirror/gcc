/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#include "avx512f-helper.h"

typedef __attribute__((__vector_size__ (16))) unsigned V;

short s;

V
foo ()
{
  return ~(-(V){ 0, 0, 0, 1 } <= s);
}

void
test_128 ()
{
  V x = foo ();
  if (x[0] != 0 || x[1] != 0 || x[2] != 0 || x[3] != 0xffffffff)
    __builtin_abort();
}

void
test_256 ()
{}
