/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx -msse" } */

void f(double*d,double*e){
  for(;d<e;++d)
    *d=(*d<.5)?.7:0;
}

/* { dg-final { scan-assembler-not "andnpd" } } */
/* { dg-final { scan-assembler-not "orpd" } } */
