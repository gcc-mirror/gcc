/* Testcase to check generation of a SH2A specific instruction for
   'MOVRT Rn'.  */
/* { dg-do assemble {target sh*-*-*}}  */
/* { dg-options "-O1" }  */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "movrt"} }  */

int
foo (void)
{
  int a, b, g, stop;
  if (stop = ((a + b) % 2 != g))
    ;
  return stop;
}
