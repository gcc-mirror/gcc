/* PR tree-optimization/110852 */
/* { dg-options "-O1 -fno-tree-fre" } */

unsigned i, j;

unsigned
foo (void)
{
  unsigned u = __builtin_expect (i, 0);
  return 4 - (u < (j || i));
}
