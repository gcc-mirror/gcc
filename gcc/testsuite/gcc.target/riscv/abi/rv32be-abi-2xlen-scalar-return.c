/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32 -mbig-endian" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

typedef unsigned int u32;
typedef unsigned long long u64;

u64
ret_one (void)
{
  return 1;
}

extern u64 ret_scalar (void);

u32
use_ret (void)
{
  return (u32) ret_scalar ();
}

/* Return values use significance ordering: a0 is the low word and a1 is the
   high word.  The caller therefore needs no move from a1 to use the low word.  */
/* { dg-final { scan-assembler {\mli[ \t]+a0,1} } } */
/* { dg-final { scan-assembler {\mli[ \t]+a1,0} } } */
/* { dg-final { scan-assembler-not {\mli[ \t]+a1,1} } } */
/* { dg-final { scan-assembler-not {\mmv[ \t]+a0,a1} } } */
