/* Make sure that we emit .cfa_restore notes for LO and HI.  */
/* { dg-options "-mips32r2 -msoft-float -g" } */
/* { dg-skip-if "forbidding a frame pointer makes this a code quallity test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 64\n" } } */
/* { dg-final { scan-assembler "\t\\\.cfi_restore 65\n" } } */
/* { dg-final { scan-assembler-not "\\\.cfi_def_cfa( |\t)" } } */
/* { dg-final { scan-assembler-not "\\\.cfi_def_cfa_register( |\t)" } } */

extern void f (void);

NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set))
v1 (void)
{
  f ();
}
