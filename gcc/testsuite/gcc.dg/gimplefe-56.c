/* { dg-do compile } */
/* { dg-options "-fgimple" } */

_Bool __GIMPLE (ssa,startwith("optimized")) g1(int i)
{
  _Bool _1;
  _Bool _2;

  __BB(2):
  _1 = i_3(D) == 1;
  _2 = ~_1;
  return _2;
}

signed char __GIMPLE (ssa,startwith("optimized")) g2(signed char i)
{
  signed char _1;
  signed char _2;

  __BB(2):
  _1 = i_3(D);
  _2 = -_1;
  return _2;
}
