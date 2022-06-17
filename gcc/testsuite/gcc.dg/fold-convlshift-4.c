/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
short foo(short x)
{
  return x << 5;
}

/* { dg-final { scan-tree-dump-not "\\(int\\)" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\(short int\\)" "optimized" } } */
