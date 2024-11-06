/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadcondmov -mtune=thead-c906 -mbranch-cost=1" } */

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
	th.mveqz	a0,zero,a1
 */

/* { dg-final { scan-assembler-times "\\ssnez\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\s(?:th\\.mveqz|th\\.mvnez)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
