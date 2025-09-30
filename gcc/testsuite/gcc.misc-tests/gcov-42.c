/* { dg-options "--coverage" } */
/* { dg-do run } */

/* We should be able to use the pragma even through a macro, also when that
   macro is defined in a different file.  */
#define SUPPRESS_COVERAGE _Pragma ("GCC suppress_coverage begin")
#define ENABLE_COVERAGE  _Pragma ("GCC suppress_coverage end")

#include "gcov-42.h"

int
fn1 (int a)
{
  int c;
  SUPPRESS_COVERAGE
  int b = a + 1;			/* count(#) */
  a *= 2;				/* count(#) */
  int d = a - 1;			/* count(#) */
  c = a+b+d;				/* count(#) */
  ENABLE_COVERAGE
  return c;				/* count(1) */
}

int
fn2 (int a)
{
  int c;
  INCLUDE_SUPPRESS_COVERAGE
  int b = a + 1;			/* count(#) */
  a *= 2;				/* count(#) */
  int d = a - 1;			/* count(#) */
  c = a+b+d;				/* count(#) */
  INCLUDE_ENABLE_COVERAGE
  return c;				/* count(1) */
}

int main ()
{
    fn1 (1);
    fn2 (2);
}

/* { dg-final { run-gcov gcov-42.c } } */
