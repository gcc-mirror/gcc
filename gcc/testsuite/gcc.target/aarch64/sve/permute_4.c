/* { dg-options "-O -msve-vector-bits=256 -fgimple" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

typedef __SVInt8_t vint8 __attribute__((arm_sve_vector_bits(256)));
typedef __SVBool_t vbool __attribute__((arm_sve_vector_bits(256)));

/*
** uzp1_h:
**	uzp1	p0\.h, p0\.h, p1\.h
**	ret
*/
vbool __GIMPLE
uzp1_h (vbool x, vbool y)
{
  vbool z;

  z = __VEC_PERM (x, y, _Literal (vint8)
		  { 0, 1, 4, 5, 8, 9, 12, 13,
		    16, 17, 20, 21, 24, 25, 28, 29,
		    32, 33, 36, 37, 40, 41, 44, 45,
		    48, 49, 52, 53, 56, 57, 60, 61 });
  return z;
}

/*
** uzp2_s:
**	uzp2	p0\.s, p0\.s, p1\.s
**	ret
*/
vbool __GIMPLE
uzp2_s (vbool x, vbool y)
{
  vbool z;

  z = __VEC_PERM (x, y, _Literal (vint8)
		  { 4, 5, 6, 7, 12, 13, 14, 15,
		    20, 21, 22, 23, 28, 29, 30, 31,
		    36, 37, 38, 39, 44, 45, 46, 47,
		    52, 53, 54, 55, 60, 61, 62, 63 });
  return z;
}

/*
** trn1_d:
**	trn1	p0\.d, p0\.d, p1\.d
**	ret
*/
vbool __GIMPLE
trn1_d (vbool x, vbool y)
{
  vbool z;

  z = __VEC_PERM (x, y, _Literal (vint8)
		  { 0, 1, 2, 3, 4, 5, 6, 7,
		    32, 33, 34, 35, 36, 37, 38, 39,
		    16, 17, 18, 19, 20, 21, 22, 23,
		    48, 49, 50, 51, 52, 53, 54, 55 });
  return z;
}

/*
** trn2_h:
**	trn2	p0\.h, p0\.h, p1\.h
**	ret
*/
vbool __GIMPLE
trn2_h (vbool x, vbool y)
{
  vbool z;

  z = __VEC_PERM (x, y, _Literal (vint8)
		  { 2, 3, 34, 35, 6, 7, 38, 39,
		    10, 11, 42, 43, 14, 15, 46, 47,
		    18, 19, 50, 51, 22, 23, 54, 55,
		    26, 27, 58, 59, 30, 31, 62, 63 });
  return z;
}

/*
** zip1_d:
**	zip1	p0\.d, p0\.d, p1\.d
**	ret
*/
vbool __GIMPLE
zip1_d (vbool x, vbool y)
{
  vbool z;

  z = __VEC_PERM (x, y, _Literal (vint8)
		  { 0, 1, 2, 3, 4, 5, 6, 7,
		    32, 33, 34, 35, 36, 37, 38, 39,
		    8, 9, 10, 11, 12, 13, 14, 15,
		    40, 41, 42, 43, 44, 45, 46, 47 });
  return z;
}

/*
** zip2_s:
**	zip2	p0\.s, p0\.s, p1\.s
**	ret
*/
vbool __GIMPLE
zip2_s (vbool x, vbool y)
{
  vbool z;

  z = __VEC_PERM (x, y, _Literal (vint8)
		  { 16, 17, 18, 19, 48, 49, 50, 51,
		    20, 21, 22, 23, 52, 53, 54, 55,
		    24, 25, 26, 27, 56, 57, 58, 59,
		    28, 29, 30, 31, 60, 61, 62, 63 });
  return z;
}
