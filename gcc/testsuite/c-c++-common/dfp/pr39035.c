/* { dg-options "-O" } */

/* DFP TR 24732 == WG14 / N1176, N1312 */
/* Based on a test from Fred Tydeman.  */

#include "dfp-dbg.h"

/* Test runtime computations.  */

void
runtime32 (void)
{
  volatile _Decimal32 d;
  d = 0.0DF;
  if (d)
    FAILURE
}

void
runtime64 (void)
{
  volatile _Decimal64 d;
  d = 0.0DD;
  if (d)
    FAILURE
}

void
runtime128 (void)
{
  volatile _Decimal128 d;
  d = 0.0DL;
  if (d)
    FAILURE
}

void
fold32 (void)
{
  if (0.0DF)
    FAILURE
}

void
fold64 (void)
{
  if (0.0DD)
    FAILURE
}

void
fold128 (void)
{
  if (0.0DL)
    FAILURE
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

  FINISH
}
