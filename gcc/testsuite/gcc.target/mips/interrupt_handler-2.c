/* Make sure that we emit .cfa_restore notes for LO and HI.  */
/* { dg-options "-mips32r2 -msoft-float -O -g" } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 64\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 65\n" } } */
/* { dg-final { scan-assembler-not "\\\.cfi_def_cfa( |\t)" } } */
/* { dg-final { scan-assembler-not "\\\.cfi_def_cfa_register( |\t)" } } */
/* { dg-skip-if "PR target/50580" { mips-sgi-irix6* } } */

extern void f (void);

NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set))
v1 (void)
{
  f ();
}
