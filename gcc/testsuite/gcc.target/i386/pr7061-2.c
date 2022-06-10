/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */
float im(float _Complex a) { return __imag__ a; }
/* { dg-final { scan-assembler-not "movss" } } */
/* { dg-final { scan-assembler-not "rsp" } } */
