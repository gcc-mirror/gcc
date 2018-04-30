/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

#include <limits.h>

/* Original testcase from PR.  */

int foo1 (short x) {
  return (double) x != 0;
  /* { dg-final { scan-tree-dump "return (<retval> = )?x != 0" "original" } } */
}

int foo2 (short x) {
  return (float) x != 0;
  /* { dg-final { scan-tree-dump "return (<retval> = )?x != 0" "original" } } */
}

int foo3 (int x) {
  return (double) x != 0;
  /* { dg-final { scan-tree-dump "return (<retval> = )?x != 0" "original" } } */
}

/* Tests when RHS is within range of integer type.  */

void in_range (unsigned short x)
{
  {
    volatile int in_range_1;
    in_range_1 = (float) x > 100.0f;
    /* { dg-final { scan-tree-dump "in_range_1 = x > 100" "original" } } */
  }

  {
    volatile int in_range_2;
    in_range_2 = (float) x < 100.0f;
    /* { dg-final { scan-tree-dump "in_range_2 = x <= 99" "original" } } */
  }

  {
    volatile int in_range_3;
    in_range_3 = (float) x > 100.5f;
    /* { dg-final { scan-tree-dump "in_range_3 = x (>= 101|> 100)" "original" } } */
  }

  {
    volatile int in_range_4;
    in_range_4 = (float) x < 100.5f;
    /* { dg-final { scan-tree-dump "in_range_4 = x <= 100" "original" } } */
  }

  {
    volatile int in_range_5;
    in_range_5 = (float) x == 100.0f;
    /* { dg-final { scan-tree-dump "in_range_5 = x == 100" "original" } } */
  }

  {
    volatile int in_range_6;
    in_range_6 = (float) x != 100.0f;
    /* { dg-final { scan-tree-dump "in_range_6 = x != 100" "original" } } */
  }

  {
    volatile int in_range_7;
    in_range_7 = (float) x == 100.5f;
    /* { dg-final { scan-tree-dump "in_range_7 = 0" "original" } } */
  }

  {
    volatile int in_range_8;
    in_range_8 = (float) x != 100.5f;
    /* { dg-final { scan-tree-dump "in_range_8 = 1" "original" } } */
  }
}

/* Tests for cases where RHS is out of range of integer type.  */

void out_range (unsigned short x)
{
  {
    volatile int out_range_1;
    out_range_1 = (float) x > -100.5f;
    /* { dg-final { scan-tree-dump "out_range_1 = 1" "original" } } */
  }

  {
    volatile int out_range_2;
    out_range_2 = (float) x >= -100.5f;
    /* { dg-final { scan-tree-dump "out_range_2 = 1" "original" } } */
  }

  {
    volatile int out_range_3;
    out_range_3 = (float) x < -100.5f;
    /* { dg-final { scan-tree-dump "out_range_3 = 0" "original" } } */
  }

  {
    volatile int out_range_4;
    out_range_4 = (float) x <= -100.5f;
    /* { dg-final { scan-tree-dump "out_range_4 = 0" "original" } } */
  }

  {
    volatile int out_range_5;
    out_range_5 = (float) x == -100.5f;
    /* { dg-final { scan-tree-dump "out_range_5 = 0" "original" } } */
  }

  {
    volatile int out_range_6;
    out_range_6 = (float) x != -100.5f;
    /* { dg-final { scan-tree-dump "out_range_6 = 1" "original" } } */
  }
}

/* Tests when RHS is at boundary of integer type.  */

void lo_bounds (unsigned short x)
{
  {
    volatile int lo_bounds_1;
    lo_bounds_1 = (float) x > 0x0;
    /* { dg-final { scan-tree-dump "lo_bounds_1 = x (>|!=) 0" "original" } } */
  }

  {
    volatile int lo_bounds_2;
    lo_bounds_2 = (float) x >= 0x0;
    /* { dg-final { scan-tree-dump "lo_bounds_2 = 1" "original" } } */
  }

  {
    volatile int lo_bounds_3;
    lo_bounds_3 = (float) x < 0x0;
    /* { dg-final { scan-tree-dump "lo_bounds_3 = 0" "original" } } */
  }

  {
    volatile int lo_bounds_4;
    lo_bounds_4 = (float) x <= 0x0;
    /* { dg-final { scan-tree-dump "lo_bounds_4 = x (<=|==) 0" "original" } } */
  }

  {
    volatile int lo_bounds_5;
    lo_bounds_5 = (float) x > 0x0 - 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_5 = 1" "original" } } */
  }

  {
    volatile int lo_bounds_6;
    lo_bounds_6 = (float) x >= 0x0 - 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_6 = 1" "original" } } */
  }

  {
    volatile int lo_bounds_7;
    lo_bounds_7 = (float) x < 0x0 - 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_7 = 0" "original" } } */
  }

  {
    volatile int lo_bounds_8;
    lo_bounds_8 = (float) x <= 0x0 - 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_8 = 0" "original" } } */
  }

  {
    volatile int lo_bounds_9;
    lo_bounds_9 = (float) x > 0x0 + 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_9 = x (>= 1|!= 0)" "original" } } */
  }

  {
    volatile int lo_bounds_10;
    lo_bounds_10 = (float) x >= 0x0 + 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_10 = x (>= 1|!= 0)" "original" } } */
  }

  {
    volatile int lo_bounds_11;
    lo_bounds_11 = (float) x < 0x0 + 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_11 = x (<=|==) 0" "original" } } */
  }

  {
    volatile int lo_bounds_12;
    lo_bounds_12 = (float) x <= 0x0 + 0.5f;
    /* { dg-final { scan-tree-dump "lo_bounds_12 = x (<=|==) 0" "original" } } */
  }
}

void hi_bounds (unsigned short x)
{
  {
    volatile int hi_bounds_1;
    hi_bounds_1 = (float) x > USHRT_MAX;
    /* { dg-final { scan-tree-dump "hi_bounds_1 = 0" "original" } } */
  }

  {
    volatile int hi_bounds_2;
    hi_bounds_2 = (float) x >= USHRT_MAX;
    /* { dg-final { scan-tree-dump "hi_bounds_2 = x (>=|==) 65535" "original" } } */
  }

  {
    volatile int hi_bounds_3;
    hi_bounds_3 = (float) x < USHRT_MAX;
    /* { dg-final { scan-tree-dump "hi_bounds_3 = x (<|!=) 65535" "original" } } */
  }

  {
    volatile int hi_bounds_4;
    hi_bounds_4 = (float) x <= USHRT_MAX;
    /* { dg-final { scan-tree-dump "hi_bounds_4 = 1" "original" } } */
  }

  {
    volatile int hi_bounds_5;
    hi_bounds_5 = (float) x > USHRT_MAX - 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_5 = x (>=|==) 65535" "original" } } */
  }

  {
    volatile int hi_bounds_6;
    hi_bounds_6 = (float) x >= USHRT_MAX - 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_6 = x (>=|==) 65535" "original" } } */
  }

  {
    volatile int hi_bounds_7;
    hi_bounds_7 = (float) x < USHRT_MAX - 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_7 = x (<= 65534|!= 65535)" "original" } } */
  }

  {
    volatile int hi_bounds_8;
    hi_bounds_8 = (float) x <= USHRT_MAX - 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_8 = x (<= 65534|!= 65535)" "original" } } */
  }

  {
    volatile int hi_bounds_9;
    hi_bounds_9 = (float) x > USHRT_MAX + 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_9 = 0" "original" } } */
  }

  {
    volatile int hi_bounds_10;
    hi_bounds_10 = (float) x >= USHRT_MAX + 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_10 = 0" "original" } } */
  }

  {
    volatile int hi_bounds_11;
    hi_bounds_11 = (float) x < USHRT_MAX + 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_11 = 1" "original" } } */
  }

  {
    volatile int hi_bounds_12;
    hi_bounds_12 = (float) x <= USHRT_MAX + 0.5f;
    /* { dg-final { scan-tree-dump "hi_bounds_12 = 1" "original" } } */
  }
}

/* Tests with non-finite float consts.  */

void nonfinite (unsigned short x)
{
#define INFINITY __builtin_inff ()

  {
    volatile int nonfinite_1;
    nonfinite_1 = (float) x > INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_1 = 0" "original" } } */
  }

  {
    volatile int nonfinite_2;
    nonfinite_2 = (float) x >= INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_2 = 0" "original" } } */
  }

  {
    volatile int nonfinite_3;
    nonfinite_3 = (float) x < INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_3 = 1" "original" } } */
  }

  {
    volatile int nonfinite_4;
    nonfinite_4 = (float) x <= INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_4 = 1" "original" } } */
  }

  {
    volatile int nonfinite_5;
    nonfinite_5 = (float) x > -INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_5 = 1" "original" } } */
  }

  {
    volatile int nonfinite_6;
    nonfinite_6 = (float) x >= -INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_6 = 1" "original" } } */
  }

  {
    volatile int nonfinite_7;
    nonfinite_7 = (float) x < -INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_7 = 0" "original" } } */
  }

  {
    volatile int nonfinite_8;
    nonfinite_8 = (float) x <= -INFINITY;
    /* { dg-final { scan-tree-dump "nonfinite_8 = 0" "original" } } */
  }

#define QNAN __builtin_nanf ("0")

  /* Even for qNaNs, only == and != are quiet.  */

  {
    volatile int nonfinite_9;
    nonfinite_9 = (float) x == QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_9 = 0" "original" } } */
  }

  {
    volatile int nonfinite_10;
    nonfinite_10 = (float) x != QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_10 = 1" "original" } } */
  }
}

/* { dg-final { scan-tree-dump-not "\\(float\\)" "original" } } */
/* { dg-final { scan-tree-dump-not "\\(double\\)" "original" } } */
