/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/106164 */
/* PR tree-optimization/111456 */

_Bool iu(int a)
{
  _Bool t = a == 0;
  unsigned t1 = a;
  _Bool t2 = t1 >= 3;
  return t & t2;
}

_Bool is(int a)
{
  _Bool t = a == 0;
  short t1 = a;
  _Bool t2 = t1 >= 3;
  return t & t2;
}

/* { dg-final { scan-tree-dump-times "return 0" 2 "optimized" } } */
