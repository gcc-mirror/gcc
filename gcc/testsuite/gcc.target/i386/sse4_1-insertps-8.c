/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx" } */

typedef int v4si __attribute__ ((__vector_size__ (16)));

v4si i00(v4si x) { return (v4si){x[0],0,0,0}; }
v4si i10(v4si x) { return (v4si){x[1],0,0,0}; }
v4si i20(v4si x) { return (v4si){x[2],0,0,0}; }
v4si i30(v4si x) { return (v4si){x[3],0,0,0}; }

v4si i01(v4si x) { return (v4si){0,x[0],0,0}; }
v4si i11(v4si x) { return (v4si){0,x[1],0,0}; }
v4si i21(v4si x) { return (v4si){0,x[2],0,0}; }
v4si i31(v4si x) { return (v4si){0,x[3],0,0}; }

v4si i02(v4si x) { return (v4si){0,0,x[0],0}; }
v4si i12(v4si x) { return (v4si){0,0,x[1],0}; }
v4si i22(v4si x) { return (v4si){0,0,x[2],0}; }
v4si i32(v4si x) { return (v4si){0,0,x[3],0}; }

v4si i03(v4si x) { return (v4si){0,0,0,x[0]}; }
v4si i13(v4si x) { return (v4si){0,0,0,x[1]}; }
v4si i23(v4si x) { return (v4si){0,0,0,x[2]}; }
v4si i33(v4si x) { return (v4si){0,0,0,x[3]}; }

/* { dg-final { scan-assembler-times "\tv?insertps\t" 14 } } */
/* { dg-final { scan-assembler-times "\tv?pslldq\t" 1 } } */
/* { dg-final { scan-assembler-times "\tv?psrldq\t" 1 } } */
/* { dg-final { scan-assembler-not "\tv?movdqa\t" } } */
/* { dg-final { scan-assembler-not "\tv?movss\t" } } */
/* { dg-final { scan-assembler-not "\tv?pxor\t" } } */
