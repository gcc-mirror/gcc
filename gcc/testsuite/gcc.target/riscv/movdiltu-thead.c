/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_xtheadcondmov -mtune=thead-c906 -mbranch-cost=2 -fdump-rtl-ce1" } */

typedef unsigned int __attribute__ ((mode (DI))) int_t;

int_t
movdiltu (int_t w, int_t x, int_t y, int_t z)
{
  return w < x ? y : z;
}

/* Expect branchless assembly like:

	sltu	a0,a0,a1
	th.mveqz	a2,a3,a0
	mv	a0,a2
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:sgtu|sltu)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\s(?:th\\.mveqz|th\\.mvnez)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:bgeu|bgtu|bleu|bltu)\\s" } } */
