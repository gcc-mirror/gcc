/* PR middle-end/30322 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a)
{
  return a + ~a;
}

int test2(int b)
{
  return ~b + b;
}

unsigned int test3(unsigned int c)
{
  return c + ~c;
}

unsigned int test4(unsigned int d)
{
  return ~d + d;
}

/* { dg-final { scan-tree-dump-times "\\+ a" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "\\+ b" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "\\+ c" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "\\+ d" 0 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */

