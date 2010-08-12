/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-reassoc1" } */
unsigned int foo(unsigned int a, unsigned int b, unsigned int c, unsigned int d)
{
  /* Should be transformed into a + c + 8 */
  unsigned int e = a + 3;
  unsigned int f = c + 5;
  unsigned int g = e + f;
  return g;
}

/* { dg-final { scan-tree-dump-times "\\\+ 8" 1 "reassoc1"} } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
