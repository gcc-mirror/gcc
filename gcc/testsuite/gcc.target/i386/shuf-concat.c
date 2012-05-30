/* { dg-do compile } */
/* { dg-options "-O -msse2 -mfpmath=sse" } */

typedef double v2df __attribute__ ((__vector_size__ (16)));

v2df f(double d,double e){
  v2df x={-d,d};
  v2df y={-e,e};
  return __builtin_ia32_shufpd(x,y,1);
}

/* { dg-final { scan-assembler-not "\tv?shufpd\[ \t\]" } } */
/* { dg-final { scan-assembler-times "\tv?unpcklpd\[ \t\]" 1 } } */
