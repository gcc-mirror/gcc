/* { dg-do compile } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra -Wno-unused" } */

/* Initializers of TREE_STATICs aren't instrumented.
   But don't ICE on 'em.  */

int A[2];
int *gp = &A[4];
int *gpi;

int
main (void)
{
  gpi = &A[4];
  static int *pi = &A[4];
  return 0;
}
