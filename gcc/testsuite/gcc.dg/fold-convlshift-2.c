/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int foo(unsigned char c)
{
  int t1 = c;
  int t2 = t1 << 8;
  return t2;
}

int bar(unsigned char c)
{
  unsigned int t1 = c;
  unsigned int t2 = t1 << 8;
  return t2;
}

/* { dg-final { scan-tree-dump-times "\\(int\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\(unsigned int\\)" 1 "optimized" } } */

