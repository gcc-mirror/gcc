/* PR tree-optimization/56098 */
/* { dg-do compile } */
/* { dg-options "-O2 -fhoist-adjacent-loads -fdump-tree-optimized" } */

struct S { volatile int i; volatile int j; };

int
bar (struct S *x, int y)
{
  int r;
  if (y)
    r = x->i;
  else
    r = x->j;
  return r;
}

/* { dg-final { scan-tree-dump-not "r_\[0-9]* =.v. \[^\n\r]*;\[\n\r]*  r_\[0-9]* =.v. " "optimized" } } */
