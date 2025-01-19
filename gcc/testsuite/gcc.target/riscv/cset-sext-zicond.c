/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc_zicond -mtune=rocket -mbranch-cost=1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mtune=rocket -mbranch-cost=1" { target { rv32 } } } */

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

/* { dg-final { scan-assembler-times "\\ssnez\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.eqz\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
