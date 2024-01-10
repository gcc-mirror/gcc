/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc_zicond -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mtune=rocket -mbranch-cost=3 -fdump-rtl-ce1" { target { rv32 } } } */

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
	czero.eqz	a0,a0,a1
 */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 1 "ce1" { xfail { rv32 && { any-opts "-O1" "-Os" "-Oz" } } } } } */
/* { dg-final { scan-assembler-times "\\ssnez\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.eqz\\s" 1 { xfail { rv32 && { any-opts "-O1" "-Os" "-Oz" } } } } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" { xfail { rv32 && { any-opts "-O1" "-Os" "-Oz" } } } } } */
