/* Verify straight-line strength reduction for simple pointer subtraction.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int*
f (int s, int *c)
{
  int a1, a2, a3, *x1, *x2, *x3;

  a1 = 2 * s;
  x1 = c - a1;
  a2 = 4 * s;
  x2 = c - a2;
  a3 = 6 * s;
  x3 = c - a3;
  return x1 ? x2 : x3;
}

/* There are 4 ' * ' instances in the decls (since "int * iftmp.0;" is
   added), 1 parm, 2 in the code.  The second one in the code can be
   a widening mult.  */
/* { dg-final { scan-tree-dump-times " w?\\* " 7 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
