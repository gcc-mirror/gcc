/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=3 -mmovcc -fdump-rtl-ce1" } */

typedef int __attribute__ ((mode (DI))) int_t;

int_t
adddine (int_t w, int_t x, int_t y, int_t z)
{
  return w != x ? y + z : y;
}

/* Expect branched assembly like:

	beq	a0,a1,.L3
	add	a0,a2,a3
	ret
.L3:
	mv	a0,a2
	ret
 */

/* { dg-final { scan-rtl-dump-not "Conversion succeeded on pass \[0-9\]+\\." "ce1" } } */
/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through" "ce1" } } */
/* { dg-final { scan-assembler-times "\\s(?:beq|bne)\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\ssub\\s" } } */
/* { dg-final { scan-assembler-not "\\s(?:seqz|snez)\\s" } } */
