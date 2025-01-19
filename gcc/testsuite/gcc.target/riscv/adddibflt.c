/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=2 -mmovcc -ffinite-math-only -fdump-rtl-ce1" } */

typedef int __attribute__ ((mode (DI))) int_t;

int_t
adddiflt (double w, double x, int_t y, int_t z)
{
  return w < x ? y + z : y;
}

/* Expect branched assembly like:

	flt.d	a5,fa0,fa1
	beq	a5,zero,.L2
	add	a0,a0,a1
.L2:
 */

/* { dg-final { scan-rtl-dump-not "Conversion succeeded on pass \[0-9\]+\\." "ce1" } } */
/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through" "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:fge\\.d|fgt\\.d|fle\\.d|flt\\.d)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\s(?:beq|bne)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
