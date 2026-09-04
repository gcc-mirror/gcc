/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbkb -O2 -mabi=lp64" } */
/* { dg-skip-if "needs -O2" { *-*-* } { "-O0" "-O1" "-O3" "-Og" "-Os" "-Oz" "-flto" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** br8:
**	brev8	a0,a0
**	andi	a0,a0,0xff
**	ret
**  ...
*/
unsigned char
br8 (unsigned char x)
{
  return __builtin_bitreverse8 (x);
}

/*
** br16:
**	rev8	a0,a0
**	brev8	t0,a0
**	srli	a0,t0,48
**	ret
**  ...
*/
unsigned short
br16 (unsigned short x)
{
  return __builtin_bitreverse16 (x);
}

/*
** br32:
**	rev8	a0,a0
**	brev8	t0,a0
**	srai	a0,t0,32
**	ret
**  ...
*/
unsigned
br32 (unsigned x)
{
  return __builtin_bitreverse32 (x);
}

/*
** br64:
**	rev8	a0,a0
**	brev8	a0,a0
**	ret
**  ...
*/
unsigned long long
br64 (unsigned long long x)
{
  return __builtin_bitreverse64 (x);
}

/*
** br128:
**	rev8	a5,a0
**	rev8	a0,a1
**	brev8	a0,a0
**	brev8	a1,a5
**	ret
**  ...
*/
unsigned __int128
br128 (unsigned __int128 x)
{
  return __builtin_bitreverse128 (x);
}
