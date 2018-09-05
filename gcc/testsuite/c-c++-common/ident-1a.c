/* PR testsuite/52665
 * Make sure scan-assembler turns off .ident  */
/* { dg-do compile } */
int i;

/* { dg-final { scan-assembler "GCC: " { xfail *-*-* } } } */
/* The testsuite saw scan-assembler and turned off .ident so the above
 * has to fail for proper operation.  */
