/* Test promotion of __fp16 to double as arguments to unprototyped
   function in another compilation unit.  */

/* { dg-do run } */
/* { dg-options "-mfp16-format=ieee -std=c17" } */
/* { dg-additional-sources "fp16-unprototyped-2.c" } */

#include <stdlib.h>

extern int f ();

static __fp16 x = 42.0;
static __fp16 y = -42.0;

int
main (void)
{
  if (!f (x, y))
    abort ();
  return 0;
}
