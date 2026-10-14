/* { dg-do run } */
/* { dg-options "-O2 -fgimple" } */

/* M_1 <= 1 is always true, so it is not equivalent to x == y although
   M_1 is x != y.  The fold happens in reassoc.  */

int __GIMPLE (ssa,startwith("reassoc1")) __attribute__((noipa))
f (int x, int y)
{
  _Bool M;
  _Bool I1;
  _Bool I2;
  _Bool V;
  _Bool W;
  _Bool T;
  _Bool Z;
  int r;

  __BB(2):
  M_1 = x_10(D) != y_11(D);
  I1_5 = x_10(D) == y_11(D);
  I2_6 = M_1 <= _Literal (_Bool) 1;
  V_7 = I1_5 & I2_6;
  W_14 = V_7 == _Literal (_Bool) 0;
  T_15 = M_1 != _Literal (_Bool) 0;
  Z_17 = W_14 & T_15;
  r_8 = (int) Z_17;
  return r_8;
}

int
main (void)
{
  if (f (1, 2) != 1 || f (2, 2) != 0)
    __builtin_abort ();
  return 0;
}
