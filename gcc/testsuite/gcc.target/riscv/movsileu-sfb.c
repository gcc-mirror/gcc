/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-7-series -mbranch-cost=1 -fdump-rtl-ce1" { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mtune=sifive-7-series -mbranch-cost=1 -fdump-rtl-ce1" { target { rv32 } } } */

typedef unsigned int __attribute__ ((mode (SI))) int_t;

int_t
movsileu (int_t w, int_t x, int_t y, int_t z)
{
  return w <= x ? y : z;
}

/* Expect short forward branch assembly like:

	bgtu	a0,a1,1f	# movcc
	mv	a3,a2
1:
	mv	a0,a3
 */

/* { dg-final { scan-rtl-dump-times "Conversion succeeded on pass 1\\." 1 "ce1" } } */
/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:bgeu|bgtu|bleu|bltu)\\s\[^\\s\]+\\s# movcc\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:sgtu|sltu)\\s" } } */
