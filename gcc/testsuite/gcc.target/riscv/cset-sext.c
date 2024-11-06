/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv32gc -mtune=sifive-5-series -mbranch-cost=1 -mmovcc" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=1 -mmovcc" { target { rv64 } } } */

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
	snez	a0,a0
	and	a0,a1,a0
 */

/* { dg-final { scan-assembler-times "\\ssnez\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
/* { dg-final { scan-assembler-not "\\sneg\\s" } } */
