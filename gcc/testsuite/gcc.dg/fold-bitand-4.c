/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

int foo (int i)
{
  return (i * 8) & 5;
}

unsigned bar (unsigned i)
{
  return (i * 6) & 5;
}

/* { dg-final { scan-tree-dump-times "\\\&" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\\& 4;" 1 "original" } } */
