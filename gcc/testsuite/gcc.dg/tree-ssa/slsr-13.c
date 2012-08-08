/* x2 and x3 will be strength-reduced based on the same statement
   but with different variables as the stride.  Note that they will
   be strength-reduced by introducing an initializer 4*s which is
   cheaper than 5*s; similar for 4*c and 5*c.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int s, int c)
{
  int a2, a3, x1, x2, x3, x;

  x1 = c + s;
  a2 = 5 * s;
  x2 = c + a2;
  a3 = 5 * c;
  x3 = s + a3;
  x = x1 + x2 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* 4" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 5" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
