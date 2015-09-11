/* { dg-do compile } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra -Wno-array-bounds -Wno-unused" } */

/* Initializers of TREE_STATICs aren't instrumented.
   But don't ICE on 'em.  */

int A[2];
int *gp = &A[4];
int *gpi;

int
main (void)
{
  gpi = &A[4]; /* This will warn with -Warray-bounds, but only if VRP runs.  */
  static int *pi = &A[4];
  return 0;
}
