/* PR testsuite/52665
 * Make sure scan-assembler-not turns off .ident  */
/* { dg-do compile } */
/* { dg-skip-if "no assembler .ident support" { powerpc*-*-darwin* } } */
int i;

/* { dg-final { scan-assembler-not "GCC: " } } */
