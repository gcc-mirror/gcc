/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_xventanacondops -mtune=rocket -mbranch-cost=4 -fdump-rtl-ce1" } */

typedef unsigned int __attribute__ ((mode (SI))) int_t;

int_t
movsigtu (int_t w, int_t x, int_t y, int_t z)
{
  return w > x ? y : z;
}

/* Expect branchless assembly like:

	sgtu	a1,a0,a1
	vt.maskcn	a3,a3,a1
	vt.maskc	a1,a2,a1
	or	a0,a1,a3
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:sgtu|sltu)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\svt\\.maskc\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\svt\\.maskcn\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:bgeu|bgtu|bleu|bltu)\\s" } } */
