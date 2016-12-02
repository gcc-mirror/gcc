/* { dg-do compile } */
/* { dg-options "-mcmse" }  */

void
foo (void) {}

/* { dg-final { scan-assembler-not "bxns" } } */
/* { dg-final { scan-assembler "foo:" } } */
/* { dg-final { scan-assembler-not "__acle_se_foo:" } } */
