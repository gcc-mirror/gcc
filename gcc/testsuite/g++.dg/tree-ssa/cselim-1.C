/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details -fdump-tree-optimized" } */
/* PR tree-optimization/122178 */
/* cselim/cselim-limited should be able to handle clobbers. */

#include <new>

struct s1
{
  bool t;
};

void f(s1 *a, bool b)
{
  if (b)
  {
    a = new(a)s1{1};
  }
  else
  {
    a = new(a)s1{0};
  }
}

/*
  The above should be optimized in phiopt1 to:
  *a = {CLOBBER(bob)};
  a->t = b;
 */


/* { dg-final { scan-tree-dump-times "factoring out stores" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "factoring out clobber" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times " converted to straightline code" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if "  "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if "  "optimized" } } */

