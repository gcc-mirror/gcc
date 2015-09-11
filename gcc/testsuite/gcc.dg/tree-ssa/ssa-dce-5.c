/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-cddce1" } */

struct X { int i; };
struct X foo(int b)
{
  struct X x;
  if (b)
    x.i = 0;
  x.i = 1;
  return x;
}

/* { dg-final { scan-tree-dump-times "x.i =" 1 "cddce1" } } */
