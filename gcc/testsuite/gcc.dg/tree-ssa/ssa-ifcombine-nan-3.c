/* M_1 == 0 is the inverse of a < b, which is a u>= b and not a >= b.  */
/* { dg-do run } */
/* { dg-options "-O1 -fgimple -fno-trapping-math" } */
/* { dg-add-options ieee } */

int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f (double a, double b, int c)
{
  _Bool M;
  _Bool U;
  _Bool K;
  _Bool A;
  _Bool P;
  int r;

  __BB(2):
  M_1 = a_2(D) < b_3(D);
  U_5 = a_2(D) __UNORDERED b_3(D);
  K_6 = a_2(D) __UNGE b_3(D);
  A_7 = U_5 & K_6;
  if (c_4(D) != 0)
    goto __BB4;
  else
    goto __BB3;

  __BB(3):
  goto __BB4;

  __BB(4):
  P_8 = __PHI (__BB2: A_7, __BB3: _Literal (_Bool) 0);
  if (P_8 == _Literal (_Bool) 0)
    goto __BB5;
  else
    goto __BB7;

  __BB(5):
  if (M_1 == _Literal (_Bool) 0)
    goto __BB6;
  else
    goto __BB7;

  __BB(6):
  goto __BB8;

  __BB(7):
  goto __BB8;

  __BB(8):
  r_9 = __PHI (__BB6: 1, __BB7: 0);
  return r_9;
}

int
main (void)
{
  double n = __builtin_nan ("");
  if (f (n, 0.0, 0) != 1 || f (n, 0.0, 1) != 0
      || f (1.0, 0.0, 0) != 1 || f (1.0, 0.0, 1) != 1
      || f (0.0, 1.0, 0) != 0 || f (0.0, 1.0, 1) != 0)
    __builtin_abort ();
  return 0;
}
