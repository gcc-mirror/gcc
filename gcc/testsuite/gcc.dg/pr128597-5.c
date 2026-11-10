/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fgimple -fdump-tree-ifcombine-details" } */

/* Check that ifcombine doesn't merge a compare of a signed field whose
   mask clears all the extension bits, so that it's effectively
   zero-extended, with a sign-extended field of the same width.
   Starting at ifcombine keeps earlier passes from simplifying the
   compares.  */

/* S is (short) ((X >> 20) & 0xffff0fff): bits 12 to 15 are clear, whereas
   they are copies of bit 11 in C.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f1 (int x, int y)
{
  int a;
  int t;
  short s;
  int b;
  short c;
  int u;
  int v;
  int r;

  __BB(2):
  a_3 = x_1(D) >> 20;
  t_4 = a_3 & _Literal (int) -61441;
  s_5 = (short) t_4;
  b_6 = y_2(D) >> 20;
  c_7 = (short) b_6;
  if (s_5 == c_7)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_10 = x_1(D) & 255;
  v_11 = y_2(D) & 255;
  if (u_10 == v_11)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_20 = __PHI (__BB3: 1, __BB4: 0);
  return r_20;
}

/* Likewise without the narrowing conversion.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f2 (int x, int y)
{
  int a;
  int t;
  int b;
  int u;
  int v;
  int r;

  __BB(2):
  a_3 = x_1(D) >> 20;
  t_4 = a_3 & 4095;
  b_6 = y_2(D) >> 20;
  if (t_4 == b_6)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_10 = x_1(D) & 255;
  v_11 = y_2(D) & 255;
  if (u_10 == v_11)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_20 = __PHI (__BB3: 1, __BB4: 0);
  return r_20;
}

/* Like f2, with the masked compare second.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f3 (int x, int y)
{
  int a;
  int t;
  int b;
  int u;
  int v;
  int r;

  __BB(2):
  u_10 = x_1(D) & 255;
  v_11 = y_2(D) & 255;
  if (u_10 == v_11)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  a_3 = x_1(D) >> 20;
  t_4 = a_3 & 4095;
  b_6 = y_2(D) >> 20;
  if (t_4 == b_6)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_20 = __PHI (__BB3: 1, __BB4: 0);
  return r_20;
}

/* Like f2, with the masked field on the right-hand side.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f4 (int x, int y)
{
  int a;
  int t;
  int b;
  int u;
  int v;
  int r;

  __BB(2):
  a_3 = x_1(D) >> 20;
  t_4 = a_3 & 4095;
  b_6 = y_2(D) >> 20;
  if (b_6 == t_4)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_10 = x_1(D) & 255;
  v_11 = y_2(D) & 255;
  if (u_10 == v_11)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_20 = __PHI (__BB3: 1, __BB4: 0);
  return r_20;
}

/* Both fields are masked so that they are zero-extended: this can be
   combined.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f5 (int x, int y)
{
  int a;
  int t;
  int b;
  int w;
  int u;
  int v;
  int r;

  __BB(2):
  a_3 = x_1(D) >> 20;
  t_4 = a_3 & 4095;
  b_6 = y_2(D) >> 20;
  w_7 = b_6 & 4095;
  if (t_4 == w_7)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_10 = x_1(D) & 255;
  v_11 = y_2(D) & 255;
  if (u_10 == v_11)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_20 = __PHI (__BB3: 1, __BB4: 0);
  return r_20;
}

/* The masked signed field is compared with an unsigned field, both are
   zero-extended: this can be combined.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f6 (int x, int y)
{
  int a;
  int t;
  unsigned int c;
  unsigned int d;
  int e;
  int w;
  int u;
  int v;
  int r;

  __BB(2):
  a_3 = x_1(D) >> 20;
  t_4 = a_3 & 4095;
  c_5 = (unsigned int) y_2(D);
  d_6 = c_5 >> 20;
  e_7 = (int) d_6;
  if (t_4 == e_7)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_10 = x_1(D) & 255;
  w_8 = (int) c_5;
  v_11 = w_8 & 255;
  if (u_10 == v_11)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_20 = __PHI (__BB3: 1, __BB4: 0);
  return r_20;
}

#define M1 (-1)
#define X1 ((int) 0x7ff000ff)
#define X2 0x7fffffff
#define X3 ((int) 0xfff000ff)

int
main ()
{
  if (f1 (M1, M1) != 0 || f1 (X1, X1) != 1 || f1 (M1, X2) != 0
      || f1 (X3, X3) != 0)
    __builtin_abort ();
  if (f2 (M1, M1) != 0 || f2 (X1, X1) != 1 || f2 (M1, X2) != 0
      || f2 (X3, X3) != 0)
    __builtin_abort ();
  if (f3 (M1, M1) != 0 || f3 (X1, X1) != 1 || f3 (M1, X2) != 0
      || f3 (X3, X3) != 0)
    __builtin_abort ();
  if (f4 (M1, M1) != 0 || f4 (X1, X1) != 1 || f4 (X2, M1) != 0
      || f4 (X3, X3) != 0)
    __builtin_abort ();
  if (f5 (M1, M1) != 1 || f5 (X1, X1) != 1 || f5 (M1, X2) != 0
      || f5 (X3, X3) != 1 || f5 (X1, X3) != 0)
    __builtin_abort ();
  if (f6 (M1, M1) != 1 || f6 (X1, X1) != 1 || f6 (X1, X3) != 0
      || f6 (X3, X3) != 1 || f6 (M1, X3) != 1 || f6 (M1, X2) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing two comparisons" 2 "ifcombine" } } */
