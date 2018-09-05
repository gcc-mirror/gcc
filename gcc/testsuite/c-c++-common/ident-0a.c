/* PR testsuite/52665
 * Make sure scan-assembler-not turns off .ident  */
/* { dg-do compile } */
int i;

/* { dg-final { scan-assembler-not "GCC: " } } */
