/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Oz" "-Os" "-flto" } } */
/* { dg-options "-march=rv64gc_xtheadmempair -mtune=thead-c906 -funwind-tables" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadmempair -mtune=thead-c906 -funwind-tables" { target { rv32 } } } */

extern void bar (void);

void foo (void)
{
  asm volatile (";my clobber list"
		: : : "s0");
  bar ();
  asm volatile (";my clobber list"
		: : : "s0");
}

/* { dg-final { scan-assembler-times "th.sdd\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler ".cfi_offset 8, -16" { target { rv64 } } } } */
/* { dg-final { scan-assembler ".cfi_offset 1, -8" { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "th.swd\t" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler ".cfi_offset 8, -8" { target { rv32 } } } } */
/* { dg-final { scan-assembler ".cfi_offset 1, -4" { target { rv32 } } } } */

/* { dg-final { scan-assembler ".cfi_restore 1" } } */
/* { dg-final { scan-assembler ".cfi_restore 8" } } */

/* { dg-final { scan-assembler-times "th.ldd\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.lwd\t" 1 { target { rv32 } } } } */
