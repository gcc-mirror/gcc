/* Functional tests for the "target" attribute and pragma.  */

/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13 -mzarch" } */

/* { dg-final { scan-assembler-times "\t\.machine \"z13\"" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode zarch" 2 } } */

void foo(void) { }

__attribute__ ((target("arch=z10")))
void bar(void) { }

/* { dg-final { scan-assembler-times "\t\.machine push" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machine pop" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machine \"z10\"" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode push" 1 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode zarch" 2 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode pop" 1 } } */

/* { dg-final { scan-assembler-times "\t\.machine " 4 } } */
/* { dg-final { scan-assembler-times "\t\.machinemode " 4 } } */
