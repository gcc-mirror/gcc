/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" } } */
/* { dg-options "-march=rv32gc -mtune=sifive-7-series -mbranch-cost=1 -fno-ssa-phiopt -fdump-rtl-ce1" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -mtune=sifive-7-series -mbranch-cost=1 -fno-ssa-phiopt -fdump-rtl-ce1" { target { rv64 } } } */

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

/* Expect short forward branch assembly like:

	snez	a0,a0
	bne	a1,zero,1f	# movcc
	mv	a0,zero
1:
 */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 1 "ce1" } }
/* { dg-final { scan-assembler-times "\\ssnez\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sbne\\s\[^\\s\]+\\s# movcc\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sbeq\\s" } } */
