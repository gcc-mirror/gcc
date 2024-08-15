/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

#include <math.h>
void link_error();

void test1 (double x)
{
  if (x < __DBL_MAX__ && x > -__DBL_MAX__ && !__builtin_isfinite (x))
    link_error ();
}

void test2 (float x)
{
  if (x < __FLT_MAX__ && x > -__FLT_MAX__ && !__builtin_isfinite (x))
    link_error ();
}

void test3 (double x)
{
  if (__builtin_isfinite (x) && __builtin_isinf (x))
    link_error ();
}

void test4 (float x)
{
  if (__builtin_isfinite (x) && __builtin_isinf (x))
    link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
