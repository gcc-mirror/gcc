/* Verify straight-line strength reduction for simple add candidates,
   pointer version.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

void
f (int s, char *c, char *x1, char *x2, char *x3)
{
  int a1, a2, a3;

  a1 = 2 * s;
  x1 = c + a1;
  *x1 = 1;
  a2 = 4 * s;
  x2 = c + a2;
  *x2 = 2;
  a3 = 6 * s;
  x3 = c + a3;
  *x3 = 3;
}

/* There will be four ' * ' instances for the parms, one in the code.  */
/* { dg-final { scan-tree-dump-times " \\* " 5 "optimized" } } */
