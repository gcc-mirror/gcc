/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

unsigned int
my_mod (unsigned int a, unsigned int b)
{
  return a % (1 << b);
}

/* The above should be simplified to (unsigned int) ((1 << b) + -1) & a */
/* { dg-final { scan-tree-dump "& a;" "original" } } */
