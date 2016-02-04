/* Functional tests for the "target" attribute and pragma.  */

/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13 -mzarch" } */

/* { dg-final { scan-assembler-times "\t\.machine \"z13\"" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode zarch" 1 } } */

void foo(void) { }

/* { dg-final { scan-assembler-times "\t\.machine " 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode " 1 } } */
