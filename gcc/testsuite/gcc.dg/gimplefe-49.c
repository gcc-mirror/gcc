/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__GIMPLE (ssa) int *
bar (int i, int a, int b)
{
  int * _3;
  int *p;

__BB(2):
  if (i_24(D) <= 0)
    goto __BB3;
  else
    goto __BB4;

__BB(3):
  _3 = &a;
  goto __BB5;

__BB(4):
  p_4 = &b;
  goto __BB5;

__BB(5):
  p_5 = __PHI (__BB3: _3, __BB4: p_4);
  return p_5;
}
