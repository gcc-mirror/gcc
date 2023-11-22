/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=6 -mmovcc -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mtune=sifive-5-series -mbranch-cost=6 -mmovcc -fdump-rtl-ce1" { target { rv32 } } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
movsine (int_t w, int_t x, int_t y, int_t z)
{
  return w != x ? y : z;
}

/* Expect branchless assembly like:

	sub	a5,a0,a1
	seqz	a5,a5
	neg	a5,a5
	and	a3,a5,a3
	not	a5,a5
	and	a5,a5,a2
	or	a0,a3,a5
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\ssub\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\s(?:seqz|snez)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
