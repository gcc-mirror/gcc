/* PR testsuite/52665
 * Make sure scan-assembler-not turns off .ident unless -fident in testcase */
/* { dg-do compile } */
/* { dg-options "-fident" } */
int i;

/* { dg-final { scan-assembler-not "GCC: " { xfail *-*-* } } } */
/* The testsuite saw scan-assembler-not and turned off .ident so the above
 * has to fail for proper operation since the testsuite itself forced
 * -fident on again.  */
