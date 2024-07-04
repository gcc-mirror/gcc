/* Test that rounding double to __fp16 happens directly, using an example
   of a number that would round differently if it went from double to
   __fp16 via float.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_fp16_hw } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-std=c99" } */
/* { dg-add-options arm_fp16_alternative } */

#include <stdlib.h>

/* The original double value.  */
#define ORIG 0x1.0020008p0

/* The expected (double)((__fp16)ORIG) value.  */
#define ROUNDED 0x1.0040000p0

typedef union u {
  __fp16 f;
  unsigned short h;
} ufh;

ufh s = { ORIG };
ufh r = { ROUNDED };

double d = ORIG;

int
main (void)
{
  ufh x;

  /* Test that the rounding is correct for static initializers.  */
  if (s.h != r.h)
    abort ();

  /* Test that the rounding is correct for a casted constant expression
     not in a static initializer.  */
  x.f = (__fp16)ORIG;
  if (x.h != r.h)
    abort ();

  /* Test that the rounding is correct for a runtime conversion.  */
  x.f = (__fp16)d;
  if (x.h != r.h)
    abort ();

  return 0;
}
