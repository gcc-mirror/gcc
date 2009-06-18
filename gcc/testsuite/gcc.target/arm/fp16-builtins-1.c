/* Test type-generic builtins with __fp16 arguments.
   Except as otherwise noted, they should behave exactly
   the same as those with float arguments.  */

/* { dg-do run } */
/* { dg-options "-mfp16-format=ieee -std=gnu99" } */

#include <stdlib.h>
#include <math.h>

volatile __fp16 h1, h2;
volatile float f1, f2;

void
set1 (double x)
{
  h1 = x;
  f1 = h1;
}

void
set2 (double x, double y)
{
  h1 = x;
  f1 = h1;
  h2 = y;
  f2 = h2;
}

#define test1(p,x)				\
  set1 (x);					\
  hp = (p (h1) ? 1 : 0);			\
  fp = (p (f1) ? 1 : 0);			\
  if (hp ^ fp) abort ()

#define test2(p,x,y)				\
  set2 (x,y);					\
  hp = (p (h1, h2) ? 1 : 0);			\
  fp = (p (f1, f2) ? 1 : 0);			\
  if (hp ^ fp) abort ()

int
main (void)
{
  int hp, fp;

  test1 (__builtin_isfinite, 17.0);
  test1 (__builtin_isfinite, INFINITY);
  test1 (__builtin_isinf, -0.5);
  test1 (__builtin_isinf, INFINITY);
  test1 (__builtin_isnan, 493.0);
  test1 (__builtin_isnan, NAN);
  test1 (__builtin_isnormal, 3.14159);

  test2 (__builtin_isgreater, 5.0, 3.0);
  test2 (__builtin_isgreater, 3.0, 5.0);
  test2 (__builtin_isgreater, 73.5, 73.5);
  test2 (__builtin_isgreater, 1.0, NAN);

  test2 (__builtin_isgreaterequal, 5.0, 3.0);
  test2 (__builtin_isgreaterequal, 3.0, 5.0);
  test2 (__builtin_isgreaterequal, 73.5, 73.5);
  test2 (__builtin_isgreaterequal, 1.0, NAN);

  test2 (__builtin_isless, 5.0, 3.0);
  test2 (__builtin_isless, 3.0, 5.0);
  test2 (__builtin_isless, 73.5, 73.5);
  test2 (__builtin_isless, 1.0, NAN);

  test2 (__builtin_islessequal, 5.0, 3.0);
  test2 (__builtin_islessequal, 3.0, 5.0);
  test2 (__builtin_islessequal, 73.5, 73.5);
  test2 (__builtin_islessequal, 1.0, NAN);

  test2 (__builtin_islessgreater, 5.0, 3.0);
  test2 (__builtin_islessgreater, 3.0, 5.0);
  test2 (__builtin_islessgreater, 73.5, 73.5);
  test2 (__builtin_islessgreater, 1.0, NAN);

  test2 (__builtin_isunordered, 5.0, 3.0);
  test2 (__builtin_isunordered, 3.0, 5.0);
  test2 (__builtin_isunordered, 73.5, 73.5);
  test2 (__builtin_isunordered, 1.0, NAN);

  /* Test that __builtin_isnormal recognizes a denormalized __fp16 value,
     even if it's representable as a normalized float.  */
  h1 = 5.96046E-8;
  if (__builtin_isnormal (h1))
    abort ();

  return 0;
}
