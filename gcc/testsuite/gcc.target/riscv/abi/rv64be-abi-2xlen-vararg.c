/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64 -mbig-endian" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned long u64;
typedef unsigned __int128 u128;
typedef __builtin_va_list va_list;

extern void take_var_u128 (u64, ...);

/* A variadic u128 requires an aligned register pair.  With a0 occupied by
   the marker, a1 is skipped.  Memory-layout ordering on RV64BE puts the high
   64 bits in a2 and the low 64 bits in a3.  */

/*
** pass_var_u128:
**	...
**	mv	a2,a0
**	mv	a3,a1
**	li	a0,0
**	tail	take_var_u128(?:@plt)?
*/
void __attribute__ ((noipa))
pass_var_u128 (u64 high, u64 low)
{
  u128 x = ((u128) high << 64) | low;
  take_var_u128 (0, x);
}

/* va_arg observes the same memory ordering.  */

/*
** va_arg_u128_high:
**	...
**	ld	a0,0\([a-z][0-9]+\)
**	ret
*/
u64 __attribute__ ((noipa))
va_arg_u128_high (va_list ap)
{
  u128 x = __builtin_va_arg (ap, u128);
  return (u64) (x >> 64);
}

/*
** va_arg_u128_low:
**	...
**	ld	a0,8\([a-z][0-9]+\)
**	ret
*/
u64 __attribute__ ((noipa))
va_arg_u128_low (va_list ap)
{
  u128 x = __builtin_va_arg (ap, u128);
  return (u64) x;
}

/* Exercise va_start and the conversion from memory-layout ordering in the
   register-save area to significance ordering for a named u128 return.  */

u128 __attribute__ ((noipa))
va_u128_from_registers (u64 marker, ...)
{
  va_list ap;
  u128 x;

  __builtin_va_start (ap, marker);
  x = __builtin_va_arg (ap, u128);
  __builtin_va_end (ap);
  return x;
}

/* a1 is skipped for alignment; the value itself arrives in a2/a3.  */
/* { dg-final { scan-assembler {\msd[ \t]+a2,[0-9]+\(sp\)} } } */
/* { dg-final { scan-assembler {\msd[ \t]+a3,[0-9]+\(sp\)} } } */
