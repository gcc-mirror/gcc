/* PR middle-end/110754 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "a ={v} x" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "={v} a" "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */

int
foo (int x)
{
  volatile int a = x;
  [[gnu::assume (x == (a & 0))]];
  return x;
}
