/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fgimple -fdump-tree-ifcombine-details" } */

/* Check that ifcombine turns sign tests of masked fields whose
   extension bits are all masked out into tests that are always true or
   always false, rather than into tests of the sign bit of the field.
   Starting at ifcombine keeps earlier passes from simplifying the
   compares.  */

/* S is (short) (((X >> 20) & 0x0806) | (other bits discarded by the
   conversion)): bits 12 to 15 are clear, so S >= 0 always holds, even
   though bit 11, a copy of the sign bit of X, may be set.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f1 (int x)
{
  int a;
  int t;
  short s;
  int u;
  int r;

  __BB(2):
  a_2 = x_1(D) >> 20;
  t_3 = a_2 & _Literal (int) -63482;
  s_4 = (short) t_3;
  if (s_4 >= _Literal (short) 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_5 = x_1(D) & 267386880;
  if (u_5 == 0)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_6 = __PHI (__BB3: 1, __BB4: 0);
  return r_6;
}

/* Likewise without the narrowing conversion.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f2 (int x)
{
  int a;
  int t;
  int u;
  int r;

  __BB(2):
  a_2 = x_1(D) >> 20;
  t_3 = a_2 & 2054;
  if (t_3 >= 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_5 = x_1(D) & 267386880;
  if (u_5 == 0)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_6 = __PHI (__BB3: 1, __BB4: 0);
  return r_6;
}

/* Like f1, with the sign test as the second compare.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f3 (int x)
{
  int a;
  int t;
  short s;
  int u;
  int r;

  __BB(2):
  u_5 = x_1(D) & 267386880;
  if (u_5 == 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  a_2 = x_1(D) >> 20;
  t_3 = a_2 & _Literal (int) -63482;
  s_4 = (short) t_3;
  if (s_4 >= _Literal (short) 0)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_6 = __PHI (__BB3: 1, __BB4: 0);
  return r_6;
}

/* S < 0 never holds, so S < 0 || U != 0 is U != 0.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f4 (int x)
{
  int a;
  int t;
  short s;
  int u;
  int r;

  __BB(2):
  a_2 = x_1(D) >> 20;
  t_3 = a_2 & _Literal (int) -63482;
  s_4 = (short) t_3;
  if (s_4 < _Literal (short) 0)
    goto __BB4;
  else
    goto __BB3;

  __BB(3):
  u_5 = x_1(D) & 267386880;
  if (u_5 != 0)
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_6 = __PHI (__BB4: 1, __BB3: 0);
  return r_6;
}

/* Without a mask, S >= 0 does test bit 11 of the field, i.e., the sign
   bit of X.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f5 (int x)
{
  int a;
  short s;
  int u;
  int r;

  __BB(2):
  a_2 = x_1(D) >> 20;
  s_4 = (short) a_2;
  if (s_4 >= _Literal (short) 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_5 = x_1(D) & 267386880;
  if (u_5 == 0)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_6 = __PHI (__BB3: 1, __BB4: 0);
  return r_6;
}

int
main ()
{
  int min = -0x7fffffff - 1;
  if (f1 (min) != 1 || f1 (-1) != 0 || f1 (0) != 1 || f1 (0x00600000) != 0)
    __builtin_abort ();
  if (f2 (min) != 1 || f2 (-1) != 0 || f2 (0) != 1 || f2 (0x00600000) != 0)
    __builtin_abort ();
  if (f3 (min) != 1 || f3 (-1) != 0 || f3 (0) != 1 || f3 (0x00600000) != 0)
    __builtin_abort ();
  if (f4 (min) != 0 || f4 (-1) != 1 || f4 (0) != 0 || f4 (0x00600000) != 1)
    __builtin_abort ();
  if (f5 (min) != 0 || f5 (-1) != 0 || f5 (0) != 1 || f5 (0x00600000) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing two comparisons" 5 "ifcombine" } } */
