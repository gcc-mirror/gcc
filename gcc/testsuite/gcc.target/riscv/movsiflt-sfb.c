/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-7-series -mbranch-cost=1 -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mtune=sifive-7-series -mbranch-cost=1 -fdump-rtl-ce1" { target { rv32 } } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
movsiflt (double w, double x, int_t y, int_t z)
{
  return w < x ? y : z;
}

/* Expect short forward branch assembly like:

	flt.d	a5,fa0,fa1
	bne	a5,zero,1f	# movcc
	mv	a0,a1
1:
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through" "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:fgt\\.d|flt\\.d)\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\s(?:beq|bne)\\s\[^\\s\]+\\s# movcc\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
