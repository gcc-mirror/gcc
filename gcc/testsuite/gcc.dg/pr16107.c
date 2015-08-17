/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <math.h>

double t (double x)
{
 x = -x;
 x = cos (x);
 x = -x;
 x = cosh (x);
 return x;
}

/* { dg-final { scan-tree-dump-not "-x" "optimized" } } */

