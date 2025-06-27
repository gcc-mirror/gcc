/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector --save-temps -fno-stack-protector" } */
/* { dg-do run { target { s390_z14_hw } } } */
/* { dg-final {check-function-bodies "**" "" } } */

#include "vec-types.h"

/*
** qi_via_hi_hi:
**	vmrhh	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_hi_hi (v16qi a, v16qi b)
{
  return (v16qi){a[0], a[1], b[0], b[1], a[2], a[3], b[2], b[3],
		 a[4], a[5], b[4], b[5], a[6], a[7], b[6], b[7]};
}

/*
** qi_via_hi_lo:
**	vmrlh	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_hi_lo (v16qi a, v16qi b)
{
  return (v16qi){a[8], a[9], b[8], b[9], a[10], a[11], b[10], b[11],
		 a[12], a[13], b[12], b[13], a[14], a[15], b[14], b[15]};
}

/*
** qi_via_si_hi:
**	vmrhf	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_si_hi (v16qi a, v16qi b)
{
  return (v16qi){a[0], a[1], a[2], a[3], b[0], b[1], b[2], b[3],
		 a[4], a[5], a[6], a[7], b[4], b[5], b[6], b[7]};
}

/*
** qi_via_si_lo:
**	vmrlf	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_si_lo (v16qi a, v16qi b)
{
  return (v16qi){a[8], a[9], a[10], a[11], b[8], b[9], b[10], b[11],
		 a[12], a[13], a[14], a[15], b[12], b[13], b[14], b[15]};
}

/*
** qi_via_di_hi:
**	vmrhg	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_di_hi (v16qi a, v16qi b)
{
  return (v16qi){a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		 b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]};
}

/*
** qi_via_di_lo:
**	vmrlg	%v24,%v24,%v26
**	br	%r14
*/
v16qi __attribute__((noinline,noipa))
qi_via_di_lo (v16qi a, v16qi b)
{
  return (v16qi){a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		 b[8], b[9], b[10], b[11], b[12], b[13], b[14], b[15]};
}

/*
** hi_via_si_hi:
**	vmrhf	%v24,%v24,%v26
**	br	%r14
*/
v8hi __attribute__((noinline,noipa))
hi_via_si_hi (v8hi a, v8hi b)
{
  return (v8hi){a[0], a[1], b[0], b[1], a[2], a[3], b[2], b[3]};
}

/*
** hi_via_si_lo:
**	vmrlf	%v24,%v24,%v26
**	br	%r14
*/
v8hi __attribute__((noinline,noipa))
hi_via_si_lo (v8hi a, v8hi b)
{
  return (v8hi){a[4], a[5], b[4], b[5], a[6], a[7], b[6], b[7]};
}

/*
** hi_via_di_hi:
**	vmrhg	%v24,%v24,%v26
**	br	%r14
*/
v8hi __attribute__((noinline,noipa))
hi_via_di_hi (v8hi a, v8hi b)
{
  return (v8hi){a[0], a[1], a[2], a[3], b[0], b[1], b[2], b[3]};
}

/*
** hi_via_di_lo:
**	vmrlg	%v24,%v24,%v26
**	br	%r14
*/
v8hi __attribute__((noinline,noipa))
hi_via_di_lo (v8hi a, v8hi b)
{
  return (v8hi){a[4], a[5], a[6], a[7], b[4], b[5], b[6], b[7]};
}

/*
** si_via_di_hi:
**	vmrhg	%v24,%v24,%v26
**	br	%r14
*/
v4si __attribute__((noinline,noipa))
si_via_di_hi (v4si a, v4si b)
{
  return (v4si){a[0], a[1], b[0], b[1]};
}

/*
** si_via_di_lo:
**	vmrlg	%v24,%v24,%v26
**	br	%r14
*/
v4si __attribute__((noinline,noipa))
si_via_di_lo (v4si a, v4si b)
{
  return (v4si){a[2], a[3], b[2], b[3]};
}

int
main ()
{
  static const signed char e_qi_via_hi_hi[16]
    = {0, 1, 16, 17, 2, 3, 18, 19, 4, 5, 20, 21, 6, 7, 22, 23};
  static const signed char e_qi_via_hi_lo[16]
    = {8, 9, 24, 25, 10, 11, 26, 27, 12, 13, 28, 29, 14, 15, 30, 31};
  static const signed char e_qi_via_si_hi[16]
    = {0, 1, 2, 3, 16, 17, 18, 19, 4, 5, 6, 7, 20, 21, 22, 23};
  static const signed char e_qi_via_si_lo[16]
    = {8, 9, 10, 11, 24, 25, 26, 27, 12, 13, 14, 15, 28, 29, 30, 31};
  static const signed char e_qi_via_di_hi[16]
    = {0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23};
  static const signed char e_qi_via_di_lo[16]
    = {8, 9, 10, 11, 12, 13, 14, 15, 24, 25, 26, 27, 28, 29, 30, 31};

  static const short e_hi_via_si_hi[8] = {0, 1, 8, 9, 2, 3, 10, 11};
  static const short e_hi_via_si_lo[8] = {4, 5, 12, 13, 6, 7, 14, 15};
  static const short e_hi_via_di_hi[8] = {0, 1, 2, 3, 8, 9, 10, 11};
  static const short e_hi_via_di_lo[8] = {4, 5, 6, 7, 12, 13, 14, 15};

  static const int e_si_via_di_hi[4] = {0, 1, 4, 5};
  static const int e_si_via_di_lo[4] = {2, 3, 6, 7};

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

  r_qi = qi_via_hi_hi (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_hi_hi[i])
      __builtin_abort ();

  r_qi = qi_via_hi_lo (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_hi_lo[i])
      __builtin_abort ();

  r_qi = qi_via_si_hi (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_si_hi[i])
      __builtin_abort ();

  r_qi = qi_via_si_lo (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_si_lo[i])
      __builtin_abort ();

  r_qi = qi_via_di_hi (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_di_hi[i])
      __builtin_abort ();

  r_qi = qi_via_di_lo (a_qi, b_qi);
  for (i = 0; i < 16; ++i)
    if (r_qi[i] != e_qi_via_di_lo[i])
      __builtin_abort ();

  r_hi = hi_via_si_hi (a_hi, b_hi);
  for (i = 0; i < 8; ++i)
    if (r_hi[i] != e_hi_via_si_hi[i])
      __builtin_abort ();

  r_hi = hi_via_si_lo (a_hi, b_hi);
  for (i = 0; i < 8; ++i)
    if (r_hi[i] != e_hi_via_si_lo[i])
      __builtin_abort ();

  r_hi = hi_via_di_hi (a_hi, b_hi);
  for (i = 0; i < 8; ++i)
    if (r_hi[i] != e_hi_via_di_hi[i])
      __builtin_abort ();

  r_hi = hi_via_di_lo (a_hi, b_hi);
  for (i = 0; i < 8; ++i)
    if (r_hi[i] != e_hi_via_di_lo[i])
      __builtin_abort ();

  r_si = si_via_di_hi (a_si, b_si);
  for (i = 0; i < 4; ++i)
    if (r_si[i] != e_si_via_di_hi[i])
      __builtin_abort ();

  r_si = si_via_di_lo (a_si, b_si);
  for (i = 0; i < 4; ++i)
    if (r_si[i] != e_si_via_di_lo[i])
      __builtin_abort ();

  return 0;
}
