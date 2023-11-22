/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=4 -mmovcc -fdump-rtl-ce1" } */

typedef int __attribute__ ((mode (DI))) int_t;

int_t
adddine (int_t w, int_t x, int_t y, int_t z)
{
  return w != x ? y + z : y;
}

/* Expect branchless assembly like:

	sub	a1,a0,a1
	snez	a1,a1
	neg	a1,a1
	and	a1,a1,a3
	add	a0,a1,a2
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_addcc" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\ssub\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\s(?:seqz|snez)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
