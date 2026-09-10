/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx" } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

v4sf f00(v4sf x) { return (v4sf){x[0],0.0f,0.0f,0.0f}; }
v4sf f10(v4sf x) { return (v4sf){x[1],0.0f,0.0f,0.0f}; }
v4sf f20(v4sf x) { return (v4sf){x[2],0.0f,0.0f,0.0f}; }
v4sf f30(v4sf x) { return (v4sf){x[3],0.0f,0.0f,0.0f}; }

v4sf f01(v4sf x) { return (v4sf){0.0f,x[0],0.0f,0.0f}; }
v4sf f11(v4sf x) { return (v4sf){0.0f,x[1],0.0f,0.0f}; }
v4sf f21(v4sf x) { return (v4sf){0.0f,x[2],0.0f,0.0f}; }
v4sf f31(v4sf x) { return (v4sf){0.0f,x[3],0.0f,0.0f}; }

v4sf f02(v4sf x) { return (v4sf){0.0f,0.0f,x[0],0.0f}; }
v4sf f12(v4sf x) { return (v4sf){0.0f,0.0f,x[1],0.0f}; }
v4sf f22(v4sf x) { return (v4sf){0.0f,0.0f,x[2],0.0f}; }
v4sf f32(v4sf x) { return (v4sf){0.0f,0.0f,x[3],0.0f}; }

v4sf f03(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[0]}; }
v4sf f13(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[1]}; }
v4sf f23(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[2]}; }
v4sf f33(v4sf x) { return (v4sf){0.0f,0.0f,0.0f,x[3]}; }

/* { dg-final { scan-assembler-times "\tv?insertps\t" 14 } } */
/* { dg-final { scan-assembler-times "\tv?pslldq\t" 1 } } */
/* { dg-final { scan-assembler-times "\tv?psrldq\t" 1 } } */
/* { dg-final { scan-assembler-not "\tv?movdqa\t" } } */
/* { dg-final { scan-assembler-not "\tv?movss\t" } } */
/* { dg-final { scan-assembler-not "\tv?pxor\t" } } */
