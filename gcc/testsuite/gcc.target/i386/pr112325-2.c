/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"
#include "pr112325-1.c"

static void
sse2_test (void)
{
  int d[4] = { 3, 11, 22, 89};
  short w[8] = { 3, 11, 22, 89, 4, 9, 13, 7};
  char b[16] = { 3, 11, 22, 89, 4, 9, 13, 7, 2, 6, 5, 111, 163, 88, 11, 235};
  long long q[8] = { 3, 11, 22, 89, 4, 9, 13, 7};

  /* if (plus_v4si (d) != 125) */
  /*   __builtin_abort (); */

  /* if (plus_v8hi (w) != 158) */
  /*   __builtin_abort (); */

  /* if (plus_v8di (q) != 158) */
  /*   __builtin_abort (); */

  /* if (ior_v4si (d) != 95) */
  /*   __builtin_abort (); */

  /* if (ior_v8hi (w) != 95) */
  /*   __builtin_abort (); */

  /* if (ior_v16qi (b) != (char)255) */
  /*   __builtin_abort (); */

  if (ior_v8di (q) != 95)
    __builtin_abort ();

    return;
}
