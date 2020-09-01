/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mprefer-vector-width=512" } */
/* { dg-require-effective-target avx512f } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST test_512
#endif

#include "avx512f-pr96551-1.c"

void
test_512 (void)
{
  double exp[256];
  for (int i = 0; i != 256; i++)
    {
      a[i] = i * i + 3 * i + 13;
      exp[i] = a[i];
      b[i] = 0;
    }

  foo ();

  for (int i = 0; i != 256; i++)
    if (exp[i] != b[i])
      abort ();
}
