/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2" } */
/* { dg-skip-if "" { *-*-* } { "-Os" "-Oz" "-Og" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** zero_ext_20_8:
**	slli	([ast][0-9]+),[ast][0-9]+,36
**	srli	\1,\1,44
**	ret
*/
unsigned long
zero_ext_20_8 (unsigned long x)
{
  return (x >> 8) & 0xfffffUL;
}

/*
** zero_ext_16_8:
**	slli	([ast][0-9]+),[ast][0-9]+,40
**	srli	\1,\1,48
**	ret
*/
unsigned long
zero_ext_16_8 (unsigned long x)
{
  return (x >> 8) & 0xffffUL;
}

/*
** zero_ext_13_20:
**	slli	([ast][0-9]+),[ast][0-9]+,31
**	srli	\1,\1,51
**	ret
*/
unsigned long
zero_ext_13_20 (unsigned long x)
{
  return (x >> 20) & 0x1fffUL;
}
