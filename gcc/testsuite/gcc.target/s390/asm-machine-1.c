/* Functional tests for the "target" attribute and pragma.  */

/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13 -mzarch" } */

void foo(void) { }

/* { dg-final { scan-assembler-not "\t\.machine " } } */
/* { dg-final { scan-assembler-not "\t\.machinemode " } } */
