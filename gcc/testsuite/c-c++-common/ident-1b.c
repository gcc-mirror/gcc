/* PR testsuite/52665
 * Make sure scan-assembler turns off .ident unless -fident in testcase */
/* { dg-do compile } */
/* { dg-options "-fident" } */
int i;

/* { dg-final { scan-assembler "GCC: " } } */
