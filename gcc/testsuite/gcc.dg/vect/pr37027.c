/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */

#include <stdarg.h>

struct mystr
{
  int f1;
  int f2;
};

struct mystr a[16];
struct mystr b[16];
int res1, res2;


void
foo (void)
{
  int i;
  int sum1 = 0;
  int sum2 = 0;

  for (i = 0; i < 16; i++)
  {
    sum1 += a[i].f1 + b[i].f1;
    sum2 += a[i].f2 + b[i].f2;
  }

  res1 = sum1;
  res2 = sum2;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" } } */
