/* PR tree-optimization/94211 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

long
foo (long a, long b)
{
  if (__builtin_expect (b == 1, 1))
    return a;
  int e = a + 1;
  return a / b;
}
