/* { dg-do run } */
/* { dg-options "-O3 -mavx2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

unsigned int
__attribute__((noipa))
test(unsigned int a, unsigned char p[16]) {
  unsigned int res = 0;
  for (unsigned b = 0; b < a; b += 1)
    res = p[b] ? p[b] : (char) b;
  return res;
}

static void
avx2_test (void)
{
  unsigned int a = 16U;
  unsigned char p[16];
  for (int i = 0; i != 16; i++)
    p[i] = (unsigned char)128;
  unsigned int res = test (a, p);
  if (res != 128)
    __builtin_abort ();
}
