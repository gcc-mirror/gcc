/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_htm_ok } */
/* { dg-options "-O2 -mhtm -ffixed-r3 -ffixed-r4 -ffixed-r5 -ffixed-r6 -ffixed-r7 -ffixed-r8 -ffixed-r9 -ffixed-r10 -ffixed-r11 -ffixed-r12" } */

/* { dg-final { scan-assembler-not "tabort\\.\[ \t\]0" } } */

int
foo (void)
{
  return __builtin_tabort (10);
}
