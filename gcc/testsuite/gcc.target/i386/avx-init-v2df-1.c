/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx -mavx2" } */

typedef double v2df __attribute__ ((__vector_size__ (16)));

double m, n;

v2df fx0(double x) { return (v2df){ x, 0.0 }; }
v2df f0x(double x) { return (v2df){ 0.0, x }; }
v2df fxx(double x) { return (v2df){ x, x }; }
v2df fxy(double x, double y) { return (v2df){ x, y }; }

v2df fm0() { return (v2df){ m, 0.0 }; }
v2df f0m() { return (v2df){ 0.0, m }; }
v2df fmm() { return (v2df){ m, m }; }
v2df fmn() { return (v2df){ m, n }; }

/* { dg-final { scan-assembler-times "vmovq" 4 } } */
/* { dg-final { scan-assembler-times "vpslldq" 2 } } */
/* { dg-final { scan-assembler-times "vmovddup" 2 } } */
/* { dg-final { scan-assembler-times "vunpcklpd" 1 } } */
/* { dg-final { scan-assembler-times "vmovsd" 1 } } */
/* { dg-final { scan-assembler-times "vmovhpd" 1 } } */

