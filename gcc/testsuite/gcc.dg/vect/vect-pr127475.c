/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

typedef _Bool bool8 __attribute__((signed_bool_precision(8)));

void __GIMPLE (ssa)
f (unsigned char * restrict a, int n)
{
  int i;
  bool8 j;
  unsigned char _31;
  unsigned long _32;
  unsigned char * _33;

  __BB(2):
  if (n_2(D) > 0)
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  i_10 = __PHI (__BB2: 0, __BB4: i_11);
  j_20 = __PHI (__BB2: _Literal (bool8) 0, __BB4: j_21);
  _31 = (unsigned char) j_20;
  _32 = (unsigned long) i_10;
  _33 = a_1(D) + _32;
  __MEM <unsigned char> (_33) = _31;
  j_21 = j_20 + _Literal (bool8) 1;
  i_11 = i_10 + 1;
  if (i_11 < n_2(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  goto __BB3;

  __BB(5):
  return;
}
