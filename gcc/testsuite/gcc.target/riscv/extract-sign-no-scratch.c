/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2" } */
/* { dg-skip-if "" { *-*-* } { "-Os" "-Oz" "-Og" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** sign_ext_11_5:
**	slli	([ast][0-9]+),[ast][0-9]+,48
**	srai	\1,\1,53
**	ret
*/
long
sign_ext_11_5 (unsigned long x)
{
  return (long) (x << 48) >> 53;
}

/*
** sign_ext_16_8:
**	slli	([ast][0-9]+),[ast][0-9]+,40
**	srai	\1,\1,48
**	ret
*/
long
sign_ext_16_8 (unsigned long x)
{
  return (long) (x << 40) >> 48;
}

/*
** sign_ext_10_20:
**	slli	([ast][0-9]+),[ast][0-9]+,34
**	srai	\1,\1,54
**	ret
*/
long
sign_ext_10_20 (unsigned long x)
{
  return (long) (x << 34) >> 54;
}
