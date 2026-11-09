/* PR tree-optimization/121722 */
/* { dg-do run { target bitint575 } } */
/* { dg-options "-O2 -fgimple" } */

typedef unsigned _BitInt(575) U;
U res;

/* A swap of the parameters through the loop PHIs.  */

void __GIMPLE (ssa,startwith("bitintlower")) __attribute__((noipa))
swap (U a, U b, int n)
{
  U x;
  U y;
  int i;
  U r;
  U t;

  __BB(2):
  goto __BB3;

  __BB(3):
  x_1 = __PHI (__BB2: a_5(D), __BB4: y_2);
  y_2 = __PHI (__BB2: b_6(D), __BB4: x_1);
  i_3 = __PHI (__BB2: 0, __BB4: i_8);
  if (i_3 < n_7(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  i_8 = i_3 + 1;
  goto __BB3;

  __BB(5):
  t_9 = x_1 * _Literal (U) 3;
  r_10 = t_9 + y_2;
  res = r_10;
  return;
}

/* A constant store on the latch before a copy that reads the old
   value.  */

void __GIMPLE (ssa,startwith("bitintlower")) __attribute__((noipa))
cst (U a0, int n)
{
  U a;
  U b;
  U s;
  U t;
  int i;

  __BB(2):
  goto __BB3;

  __BB(3):
  a_1 = __PHI (__BB2: a0_5(D), __BB4: _Literal (U) 0);
  b_2 = __PHI (__BB2: _Literal (U) 7, __BB4: a_1);
  s_3 = __PHI (__BB2: _Literal (U) 0, __BB4: s_10);
  i_4 = __PHI (__BB2: 0, __BB4: i_11);
  if (i_4 < n_6(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  t_9 = a_1 + b_2;
  s_10 = s_3 + t_9;
  i_11 = i_4 + 1;
  goto __BB3;

  __BB(5):
  res = s_3;
  return;
}

/* A rotation whose PHIs read the old values in reverse order.  */

void __GIMPLE (ssa,startwith("bitintlower")) __attribute__((noipa))
rotr (int n)
{
  U a;
  U b;
  U c;
  U s;
  U t;
  int i;

  __BB(2):
  goto __BB3;

  __BB(3):
  a_1 = __PHI (__BB2: _Literal (U) 1, __BB4: c_3);
  b_2 = __PHI (__BB2: _Literal (U) 2, __BB4: a_1);
  c_3 = __PHI (__BB2: _Literal (U) 3, __BB4: b_2);
  s_4 = __PHI (__BB2: _Literal (U) 0, __BB4: s_10);
  i_5 = __PHI (__BB2: 0, __BB4: i_11);
  if (i_5 < n_6(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  t_9 = s_4 * _Literal (U) 10;
  s_10 = t_9 + a_1;
  i_11 = i_5 + 1;
  goto __BB3;

  __BB(5):
  res = s_4;
  return;
}

int
main ()
{
  swap (1, 2, 3);
  if (res != 7)
    __builtin_abort ();
  swap (1, 2, 2);
  if (res != 5)
    __builtin_abort ();
  cst (5, 2);
  if (res != 17)
    __builtin_abort ();
  rotr (4);
  if (res != 1321)
    __builtin_abort ();
  return 0;
}
