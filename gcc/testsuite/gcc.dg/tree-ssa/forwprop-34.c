/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

unsigned int
foo (unsigned int eax)
{
  unsigned int edx = eax & 1;
  edx ^= 1;
  eax &= -2;
  eax |= edx;
  return eax;
}

/* { dg-final { scan-tree-dump-times " = " 1 "cddce1" } } */
/* { dg-final { scan-tree-dump " = eax_\[0-9\]+\\(D\\) \\^ 1;" "cddce1" } } */
