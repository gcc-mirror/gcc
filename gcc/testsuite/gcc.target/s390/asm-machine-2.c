/* Functional tests for the "target" attribute and pragma.  */

/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13 -mzarch" } */

void foo(void) { }

__attribute__ ((target("arch=z10")))
void bar(void) { }

/* { dg-final { scan-assembler-times "\t\.machine push" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machine pop" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machine \"z10\"" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode push" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode zarch" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode pop" 1 } } */

/* { dg-final { scan-assembler-times "\t\.machine " 3 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode " 3 } } */
