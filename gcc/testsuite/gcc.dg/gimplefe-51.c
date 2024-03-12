/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__PTRDIFF_TYPE__ __GIMPLE (ssa)
foo (void *p, void *q)
{
  __PTRDIFF_TYPE__ d;

 __BB(2):
  d_3 = p_1(D) - q_2(D);
  return d_3;
}
