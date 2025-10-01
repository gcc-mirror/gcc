/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

float f0(void)
{
  float x = 0.0f;
  return x;
}
/*
** f0:
**	movi\tv0.2s, #?0
**	ret
*/

float fn1(void)
{
  float x = -0.0f;
  return x;
}
/*
** fn1:
**	movi\tv0.2s, 0x80, lsl 24
**	ret
*/

float f1(void)
{
  float x = 256.0f;
  return x;
}
/*
** f1:
**	mov\t(w[0-9]+), 1132462080
**	fmov\ts0, \1
**	ret
*/

float f2(void)
{
  float x = 123256.0f;
  return x;
}
/*
** f2:
**	mov\t(w[0-9]+), 48128
**	movk\t\1, 0x47f0, lsl 16
**	fmov\ts0, \1
**	ret
*/

float f3(void)
{
  float x = 2.0f;
  return x;
}
/*
** f3:
**	fmov\ts0, 2\.0e\+0
**	ret
*/

float f4(void)
{
  float x = -20000.1;
  return x;
}
/*
** f4:
**	mov\t(w[0-9]+), 16435
**	movk\t\1, 0xc69c, lsl 16
**	fmov\ts0, \1
**	ret
*/
