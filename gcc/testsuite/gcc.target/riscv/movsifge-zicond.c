/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_zicond -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" { target { rv32 } } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
movsifge (double w, double x, int_t y, int_t z)
{
  return w >= x ? y : z;
}

/* Expect branchless assembly like:

	fge.d	a5,fa0,fa1
	czero.eqz	a0,a0,a5
	czero.nez	a5,a1,a5
	or	a0,a5,a0
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through" "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:fge\\.d|fle\\.d)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.eqz\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.nez\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
