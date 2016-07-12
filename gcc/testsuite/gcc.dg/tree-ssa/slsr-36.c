/* Verify straight-line strength reduction for a candidate with a basis
   hidden by a phi dependences, having an unknown stride, and where the 
   phi has an argument which is a parameter.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fno-code-hoisting -fdump-tree-optimized" } */

int
f (int s, int c, int i)
{
  int a1, a3, x1, x3, x;

  a1 = i * s;
  x1 = c + a1;

  if (x1 > 6)
    i = i + 2;

  i = i + 2;
  a3 = i * s;
  x3 = c + a3;

  x = x1 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* s" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 2" 1 "optimized" } } */
