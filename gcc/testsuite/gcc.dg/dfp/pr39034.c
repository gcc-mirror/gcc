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
  volatile float v1 = 28.f, v2 = 3.f, v3 = 9.f, v4 = 31.f, v5 = 3.f, v6 = 10.f;
  float      b32 = (float)((v1/v2-v3) - (v4/v5-v6));
  _Decimal32 d32 = (float)((v1/v2-v3) - (v4/v5-v6));

  if (b32)
    FAILURE ("runtime: b32 should be zero")
  if (d32)
    FAILURE ("runtime: d32 should be zero")
}

void
runtime64 (void)
{
  volatile double v1 = 28., v2 = 3., v3 = 9., v4 = 31., v5 = 3., v6 = 10.;
  double     b64 = (double)((v1/v2-v3) - (v4/v5-v6));
  _Decimal64 d64 = (double)((v1/v2-v3) - (v4/v5-v6));

  if (b64)
    FAILURE ("runtime: b64 should be zero")
  if (d64)
    FAILURE ("runtime: d64 should be zero")
}

void
runtime128 (void)
{
  volatile long double v1 = 28.l, v2 = 3.l, v3 = 9.l,
                       v4 = 31.l, v5 = 3.l, v6 = 10.l;
  long double b128 = (long double)((v1/v2-v3) - (v4/v5-v6));
  _Decimal128 d128 = (long double)((v1/v2-v3) - (v4/v5-v6));

  if (b128)
    FAILURE ("runtime: b128 should be zero")
  if (d128)
    FAILURE ("runtime: d128 should be zero")
}

/* Test constant folding.  */

void
fold32 (void)
{
  double     d32 = (float)((28.f/3.f-9.f) - (31.f/3.f-10.f));
  _Decimal32 b32 = (float)((28.f/3.f-9.f) - (31.f/3.f-10.f));

  if (b32)
    FAILURE ("fold: b32 should be zero")
  if (d32)
    FAILURE ("fold: d32 should be zero")
}

void
fold64 (void)
{
  double     b64 = (double)((28./3.-9.) - (31./3.-10.));
  _Decimal64 d64 = (double)((28./3.-9.) - (31./3.-10.));

  if (b64)
    FAILURE ("fold: b64 should be zero")
  if (d64)
    FAILURE ("fold: d64 should be zero")
}

void
fold128 (void)
{
  long double b128 = (long double)((28./3.-9.) - (31./3.-10.));
  _Decimal128 d128 = (long double)((28./3.-9.) - (31./3.-10.));

  if (b128)
    FAILURE ("fold: b128 should be zero")
  if (d128)
    FAILURE ("fold: d128 should be zero")
}

int
main ()
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
