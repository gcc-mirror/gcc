/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv32gc -mtune=sifive-5-series -mbranch-cost=3 -mmovcc -ffinite-math-only -fdump-rtl-ce1" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=3 -mmovcc -ffinite-math-only -fdump-rtl-ce1" { target { rv64 } } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
addsifgt (double w, double x, int_t y, int_t z)
{
  return w > x ? y + z : y;
}

/* Expect branchless assembly like:

	fgt.d	a5,fa0,fa1
	neg[w]	a5,a5
	and	a5,a5,a1
	add[w]	a0,a5,a0
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_addcc" 1 "ce1" { target { rv32 } } } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\\s(?:fge\\.d|fgt\\.d|fle\\.d|flt\\.d)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
