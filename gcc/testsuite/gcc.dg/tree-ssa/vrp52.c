/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fno-tree-ccp" } */

int
foo (unsigned int i, unsigned int j)
{
  i &= 15;
  j &= 15;
  i += 1024;
  j += 2048;
  i |= j;
  return i >= 1024 + 2048;
}

/* { dg-final { scan-tree-dump-times "return 1;" 1 "vrp1" } } */
