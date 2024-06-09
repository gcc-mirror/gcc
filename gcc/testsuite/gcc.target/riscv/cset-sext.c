/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv32gc -mtune=sifive-5-series -mbranch-cost=6 -mmovcc -fdump-rtl-ce1" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=6 -mmovcc -fdump-rtl-ce1" { target { rv64 } } } */

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

	snez	a1,a1
	neg	a1,a1
	snez	a0,a0
	and	a0,a1,a0
 */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 1 "ce1" { xfail { rv32 && { any-opts "-O1" } } } } } */
/* { dg-final { scan-assembler-times "\\ssnez\\s" 2 { xfail { rv32 && { any-opts "-O1" } } } } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" { xfail { rv32 && { any-opts "-O1" } } } } } */
