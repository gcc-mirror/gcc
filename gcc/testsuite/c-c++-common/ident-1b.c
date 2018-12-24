/* PR testsuite/52665
 * Make sure scan-assembler turns off .ident unless -fident in testcase */
/* { dg-do compile } */
/* { dg-options "-fident" } */
/* { dg-skip-if "no assembler .ident support" { powerpc*-*-darwin* } } */
int i;

/* { dg-final { scan-assembler "GCC: " } } */
