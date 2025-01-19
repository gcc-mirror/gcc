/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_zicond -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" { target { rv32 } } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
movsifeq (double w, double x, int_t y, int_t z)
{
  return w == x ? y : z;
}

/* Expect branchless assembly like:

	feq.d	a5,fa0,fa1
	czero.nez	a1,a1,a5
	czero.eqz	a0,a0,a5
	or	a0,a0,a1
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\sfeq\\.d\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.eqz\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.nez\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
