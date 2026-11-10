/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fgimple -fdump-tree-ifcombine-details" } */

/* Check how ifcombine handles masks applied before a narrowing
   conversion.  Starting at ifcombine keeps earlier passes from
   simplifying the compares.  */

/* Mask bits 16 and up are discarded by the conversion: bit 15 must not
   be added to the combined test.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f1 (int x)
{
  int t;
  short s;
  int r;

  __BB(2):
  t_2 = x_1(D) & _Literal (int) -939575034;
  s_3 = (short) t_2;
  if (s_3 == _Literal (short) 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  if (t_2 >= 0)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_4 = __PHI (__BB3: 1, __BB4: 0);
  return r_4;
}

/* Bit 12 of the mask survives the conversion and tests a copy of the
   sign bit of X brought in by the shift.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f2 (int x)
{
  int a;
  int t;
  short s;
  int u;
  int r;

  __BB(2):
  a_2 = x_1(D) >> 20;
  t_3 = a_2 & 4102;
  s_4 = (short) t_3;
  if (s_4 == _Literal (short) 0)
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

/* Bits 12 to 15 of the mask are clear, so S is never negative, and the
   combined condition is always false.  */
int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f3 (int x)
{
  int a;
  int t;
  short s;
  int u;
  int r;

  __BB(2):
  a_2 = x_1(D) >> 20;
  t_3 = a_2 & 4094;
  s_4 = (short) t_3;
  if (s_4 == _Literal (short) -2)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  u_5 = x_1(D) & 267386880;
  if (u_5 == 266338304)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_6 = __PHI (__BB3: 1, __BB4: 0);
  return r_6;
}

/* Mask bits 16 and up are discarded, bits 12 to 15 are clear: the sign
   bit of X must not be tested.  */
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
  t_3 = a_2 & _Literal (int) -65530;
  s_4 = (short) t_3;
  if (s_4 == _Literal (short) 0)
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
  if (f1 (0x8000) != 1 || f1 (0x100) != 0 || f1 (min) != 0 || f1 (0) != 1)
    __builtin_abort ();
  if (f2 (min) != 0 || f2 (0) != 1 || f2 (0x00600000) != 0)
    __builtin_abort ();
  if (f3 ((int) 0xffe00000u) != 0 || f3 (0x0fe00000) != 0)
    __builtin_abort ();
  if (f4 (min) != 1 || f4 (0) != 1 || f4 (0x00600000) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing two comparisons" 4 "ifcombine" } } */
