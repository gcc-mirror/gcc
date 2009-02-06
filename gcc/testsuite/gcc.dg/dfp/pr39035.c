/* { dg-do run } */
/* { dg-options "-std=gnu99 -O" } */

/* DFP TR 24732 == WG14 / N1176, N1312 */
/* Based on a test from Fred Tydeman.  */

extern void abort (void);
int failures = 0;

#ifdef DBG
#include <stdio.h>
#define FAILURE(MSG) { printf ("line %d: %s\n", __LINE__, MSG); failures++; }
#else
#define FAILURE(MSG) failures++;
#endif

/* Test runtime computations.  */

void
runtime32 (void)
{
  volatile _Decimal32 d;
  d = 0.0DF;
  if (d)
    FAILURE ("0.0DF should be zero")
}

void
runtime64 (void)
{
  volatile _Decimal64 d;
  d = 0.0DD;
  if (d)
    FAILURE ("0.0DD should be zero")
}

void
runtime128 (void)
{
  volatile _Decimal128 d;
  d = 0.0DL;
  if (d)
    FAILURE ("0.0DL should be zero")
}

void
fold32 (void)
{
  if (0.0DF)
    FAILURE ("0.0DF should be zero")
}

void
fold64 (void)
{
  if (0.0DD)
    FAILURE ("0.0DD should be zero")
}

void
fold128 (void)
{
  if (0.0DL)
    FAILURE ("0.0DL should be zero")
}

int
main(void)
{
  runtime32 ();
  runtime64 ();
  runtime128 ();

  fold32 ();
  fold64 ();
  fold128 ();

  if (failures != 0)
    abort ();
  return 0;
}
