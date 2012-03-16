/* Make sure that we emit .cfa_restore notes for LO, HI and GPRs.  */
/* { dg-options "-mips32r2 -msoft-float -O -g" } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 1\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 2\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 3\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 4\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 5\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 6\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 7\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 8\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 9\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 10\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 11\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 12\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 13\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 14\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 15\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 24\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 25\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 31\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 64\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 65\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_def_cfa_offset 0\n" } } */
/* { dg-final { scan-assembler-not "\\\.cfi_def_cfa( |\t)" } } */
/* { dg-final { scan-assembler-not "\\\.cfi_def_cfa_register( |\t)" } } */

extern void f (void);

NOMIPS16 void __attribute__ ((interrupt))
v1 (void)
{
  f ();
}
