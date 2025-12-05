/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE (ssa)
foo (int * restrict p, int * q)
{
  int x;
  int _1;
  int _7;

  __BB(2):
  x_4 = __MEM <int> (q_3(D), 1:0);
  __MEM <int> ((int *)p_5(D), 1 :1) = 1;
  _1 = __MEM <int> (q_3(D), 1: 0);
  _7 = _1 + x_4;
  return _7;
}
