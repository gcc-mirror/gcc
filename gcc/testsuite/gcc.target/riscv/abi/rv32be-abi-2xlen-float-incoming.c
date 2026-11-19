/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32 -mbig-endian" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned int u32;
typedef unsigned long long u64;

union double_bits
{
  double value;
  u64 bits;
};

/* Under the integer calling convention, floating-point reals are passed in
   the same way as aggregates of the same size.  On RV32BE, the
   lower addressed high word is therefore in the lower-numbered GPR.  */

/*
** double_pair_low:
**	mv	a0,a1
**	ret
*/
u32 __attribute__ ((noipa))
double_pair_low (double x)
{
  union double_bits b = { .value = x };
  return (u32) b.bits;
}

/*
** double_pair_high:
**	ret
*/
u32 __attribute__ ((noipa))
double_pair_high (double x)
{
  union double_bits b = { .value = x };
  return (u32) (b.bits >> 32);
}

/*
** double_split_low:
**	...
**	lw	a0,[0-9]+\(sp\)
**	...
**	jr	ra
*/
u32 __attribute__ ((noipa))
double_split_low (u32 pad0, u32 pad1, u32 pad2, u32 pad3,
                  u32 pad4, u32 pad5, u32 pad6, double x)
{
  union double_bits b = { .value = x };
  return (u32) (b.bits);
}

/*
** double_split_high:
**	...
**	mv	a0,a7
**	...
**	jr	ra
*/
u32 __attribute__ ((noipa))
double_split_high (u32 pad0, u32 pad1, u32 pad2, u32 pad3,
                   u32 pad4, u32 pad5, u32 pad6, double x)
{
  union double_bits b = { .value = x };
  return (u32) (b.bits >> 32);
}
