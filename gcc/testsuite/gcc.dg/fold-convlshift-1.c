/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int foo(unsigned int i)
{
  int t1 = i;
  int t2 = t1 << 8;
  return t2;
}

int bar(int i)
{
  unsigned int t1 = i;
  unsigned int t2 = t1 << 8;
  return t2;
}

/* { dg-final { scan-tree-dump-not "\\(int\\)" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\(unsigned int\\)" "optimized" } } */

