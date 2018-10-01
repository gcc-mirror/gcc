/* PR testsuite/52665
 * Make sure scan-assembler-times turns off .ident unless -fident in testcase */
/* { dg-do compile } */
/* { dg-options "-fident" } */
int ident;

/* { dg-final { scan-assembler-times "GCC: " 1 } } */
