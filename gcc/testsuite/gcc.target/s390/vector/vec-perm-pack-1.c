/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector --save-temps -fno-stack-protector" } */
/* { dg-do run { target { s390_z14_hw } } } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "vec-types.h"

/*
** qi_via_hi:
**	vpkh	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_hi (v16qi a, v16qi b)
{
  return (v16qi){a[1], a[3], a[5], a[7], a[9], a[11], a[13], a[15],
		 b[1], b[3], b[5], b[7], b[9], b[11], b[13], b[15]};
}

/*
** qi_via_si:
**	vpkf	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_si (v16qi a, v16qi b)
{
  return (v16qi){a[2], a[3], a[6], a[7], a[10], a[11], a[14], a[15],
		 b[2], b[3], b[6], b[7], b[10], b[11], b[14], b[15]};
}

/*
** qi_via_di:
**	vpkg	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_di (v16qi a, v16qi b)
{
  return (v16qi){a[4], a[5], a[6], a[7], a[12], a[13], a[14], a[15],
		 b[4], b[5], b[6], b[7], b[12], b[13], b[14], b[15]};
}

/*
** hi_via_si:
**	vpkf	%v24,%v24,%v26
**	br	%r14
*/
v8hi __attribute__((noinline,noipa))
hi_via_si (v8hi a, v8hi b)
{
  return (v8hi){a[1], a[3], a[5], a[7], b[1], b[3], b[5], b[7]};
}

/*
** hi_via_di:
**	vpkg	%v24,%v24,%v26
**	br	%r14
*/
v8hi __attribute__((noinline,noipa))
hi_via_di (v8hi a, v8hi b)
{
  return (v8hi){a[2], a[3], a[6], a[7], b[2], b[3], b[6], b[7]};
}

/*
** si_via_di:
**	vpkg	%v24,%v24,%v26
**	br	%r14
*/
v4si __attribute__((noinline,noipa))
si_via_di (v4si a, v4si b)
{
  return (v4si){a[1], a[3], b[1], b[3]};
}

int
main ()
{
  static const signed char e_qi_via_hi[16]
    = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31};
  static const signed char e_qi_via_si[16]
    = {2, 3, 6, 7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27, 30, 31};
  static const signed char e_qi_via_di[16]
    = {4, 5, 6, 7, 12, 13, 14, 15, 20, 21, 22, 23, 28, 29, 30, 31};

  static const short e_hi_via_si[8] = {1, 3, 5, 7, 9, 11, 13, 15};
  static const short e_hi_via_di[8] = {2, 3, 6, 7, 10, 11, 14, 15};

  static const int e_si_via_di[4] = {1, 3, 5, 7};

  v16qi a_qi = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  v16qi b_qi = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  v8hi a_hi = {0, 1, 2, 3, 4, 5, 6, 7};
  v8hi b_hi = {8, 9, 10, 11, 12, 13, 14, 15};
  v4si a_si = {0, 1, 2, 3};
  v4si b_si = {4, 5, 6, 7};
  v16qi r_qi;
  v8hi r_hi;
  v4si r_si;
  int i;

  r_qi = qi_via_hi (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_hi[i])
      __builtin_abort ();

  r_qi = qi_via_si (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_si[i])
      __builtin_abort ();

  r_qi = qi_via_di (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_di[i])
      __builtin_abort ();

  r_hi = hi_via_si (a_hi, b_hi);
  for (i = 0; i < 8; ++i)
    if (r_hi[i] != e_hi_via_si[i])
      __builtin_abort ();

  r_hi = hi_via_di (a_hi, b_hi);
  for (i = 0; i < 8; ++i)
    if (r_hi[i] != e_hi_via_di[i])
      __builtin_abort ();

  r_si = si_via_di (a_si, b_si);
  for (i = 0; i < 4; ++i)
    if (r_si[i] != e_si_via_di[i])
      __builtin_abort ();
  return 0;
}
