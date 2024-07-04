/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc_xventanacondops -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" } */

int
foo (long a, long b)
{
  if (!b)
    return 0;
  else if (a)
    return 1;
  else
    return 0;
}

/* Expect branchless assembly like:

	snez	a0,a0
	vt.maskc	a0,a0,a1
 */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\ssnez\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\svt\\.maskc\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
