/* PR testsuite/52665
 * Make sure scan-assembler-times turns off .ident  */
/* { dg-do compile } */
/* { dg-skip-if "no assembler .ident support" { powerpc*-*-darwin* } } */
int i;

/* { dg-final { scan-assembler-times "GCC: " 0 } } */ /* internal test, keep -times 0 ! */
