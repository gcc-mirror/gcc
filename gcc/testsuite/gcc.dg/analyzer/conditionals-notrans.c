/* { dg-additional-options "-fno-analyzer-transitivity" } */
#include "analyzer-decls.h"

void test (int i, int j)
{
  if (i > 4)
    {
      __analyzer_eval (i > 4); /* { dg-warning "TRUE" } */
      __analyzer_eval (i <= 4); /* { dg-warning "FALSE" } */
      __analyzer_eval (i > 3); /* { dg-warning "TRUE" } */

      __analyzer_eval (i > 5); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i != 3); /* { dg-warning "TRUE" } */

      __analyzer_eval (i == 3); /* { dg-warning "FALSE" } */

      __analyzer_eval (i != 4); /* { dg-warning "TRUE" } */
      __analyzer_eval (i == 4); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 5); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i != 5); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i < 5); /* { dg-warning "FALSE" } */
      __analyzer_eval (i <= 5); /* { dg-warning "UNKNOWN" } */

      /* Tests of transitivity.  */
      if (j < i)
	{
	  __analyzer_eval (j < i); /* { dg-warning "TRUE" } */
	  __analyzer_eval (j <= 4); /* { dg-warning "UNKNOWN" } */
	}
      else
	{
	  __analyzer_eval (j >= i); /* { dg-warning "TRUE" } */
	  __analyzer_eval (j > 4); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
	  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
	}
    }
  else
    {
      __analyzer_eval (i > 4); /* { dg-warning "FALSE" } */
      __analyzer_eval (i <= 4); /* { dg-warning "TRUE" } */
      __analyzer_eval (i > 3); /* { dg-warning "UNKNOWN" } */

      __analyzer_eval (i > 5); /* { dg-warning "FALSE" } */
      __analyzer_eval (i != 3); /* { dg-warning "UNKNOWN" } */

      __analyzer_eval (i == 3); /* { dg-warning "UNKNOWN" } */

      __analyzer_eval (i != 4); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 4); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 5); /* { dg-warning "FALSE" } */
      __analyzer_eval (i != 5); /* { dg-warning "TRUE" } */
      __analyzer_eval (i < 5); /* { dg-warning "TRUE" } */
      __analyzer_eval (i <= 5); /* { dg-warning "TRUE" } */
    }
}

void test_2 (int i, int j, int k)
{
  if (i >= j)
    {
      __analyzer_eval (i == k); /* { dg-warning "UNKNOWN" } */
      if (j >= k)
	{
	  __analyzer_eval (i >= k); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
	  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
	  __analyzer_eval (i == k); /* { dg-warning "UNKNOWN" } */
	  if (k >= i)
	    __analyzer_eval (i == k); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
	  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
	}
    }
}

void test_3 (int flag, unsigned int i)
{
  if (!flag) {
    return;
  }

  __analyzer_eval (flag); /* { dg-warning "TRUE" } */
      
  if (i>0) {
    __analyzer_eval (i > 0); /* { dg-warning "TRUE" } */
    __analyzer_eval (flag); /* { dg-warning "TRUE" } */
  } else {
    __analyzer_eval (i <= 0); /* { dg-warning "TRUE" } */
    __analyzer_eval (flag); /* { dg-warning "TRUE" } */
  }

  __analyzer_eval (flag); /* { dg-warning "TRUE" } */
}

void test_range_int_gt_lt (int i)
{
  if (i > 3)
    if (i < 5)
      __analyzer_eval (i == 4); /* { dg-warning "TRUE" } */
}

void test_range_float_gt_lt (float f)
{
  if (f > 3)
    if (f < 5)
      __analyzer_eval (f == 4); /* { dg-warning "UNKNOWN" } */
}

void test_range_int_ge_lt (int i)
{
  if (i >= 4)
    if (i < 5)
      __analyzer_eval (i == 4); /* { dg-warning "TRUE" } */
}

void test_range_float_ge_lt (float f)
{
  if (f >= 4)
    if (f < 5)
      __analyzer_eval (f == 4); /* { dg-warning "UNKNOWN" } */
}

void test_range_int_gt_le (int i)
{
  if (i > 3)
    if (i <= 4)
      __analyzer_eval (i == 4); /* { dg-warning "TRUE" } */
}

void test_range_float_gt_le (float f)
{
  if (f > 3)
    if (f <= 4)
      __analyzer_eval (f == 4); /* { dg-warning "UNKNOWN" } */
}

void test_range_int_ge_le (int i)
{
  if (i >= 4)
    if (i <= 4)
      __analyzer_eval (i == 4); /* { dg-warning "TRUE" } */
}

void test_range_float_ge_le (float f)
{
  if (f >= 4)
    if (f <= 4)
      __analyzer_eval (f == 4); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
      /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
}

void test_float_selfcmp (float f)
{
  __analyzer_eval (f == f); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (f != f); /* { dg-warning "UNKNOWN" } */
}
