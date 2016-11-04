/* Verify straight-line strength reduction for simple pointer subtraction.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int*
f (int s, int *c, int *d)
{
  int a1, a2, a3, *x1, *x2, *x3;

  a1 = 2 * s;
  x1 = c - a1;
  a2 = 4 * s;
  x2 = c - a2;
  a3 = 6 * s;
  x3 = c - a3;
  return x1 == d ? x2 : x3;
}

/* Note that since some branch prediction heuristics changed, the
   calculations of x2 and x3 are pushed downward into the legs
   of the conditional, changing the code presented to SLSR.
   However, this proves to be a useful test for introducing an
   initializer with a cast, so we'll keep it as is.  */

/* There are 4 ' * ' instances in the decls (since "int * iftmp.0;" is
   added), 2 parms, 3 in the code.  The second one in the code may
   be a widening multiply (for example, on AArch64).  */
/* { dg-final { scan-tree-dump-times " w?\\* " 9 "optimized" } } */
