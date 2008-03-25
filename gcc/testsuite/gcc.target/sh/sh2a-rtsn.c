/* Testcase to check generation of a SH2A specific instruction for
   'RTS/N'.  */
/* { dg-do assemble {target sh*-*-*}}  */
/* { dg-options "-O0" }  */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "rts/n"} }  */

void
bar (void)
{
}
