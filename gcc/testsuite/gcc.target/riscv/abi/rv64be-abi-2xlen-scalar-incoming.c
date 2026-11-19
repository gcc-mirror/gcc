/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64 -mbig-endian" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned long u64;
typedef unsigned __int128 u128;

/* Verify that the 2 * XLEN rules are XLEN-generic rather than specific to
   RV32.  A named unsigned __int128 uses significance ordering in GPRs.  */

/*
** pair_low:
**	ret
*/
u64 __attribute__ ((noipa))
pair_low (u128 x)
{
  return (u64) x;
}

/*
** pair_high:
**	mv	a0,a1
**	ret
*/
u64 __attribute__ ((noipa))
pair_high (u128 x)
{
  return (u64) (x >> 64);
}
