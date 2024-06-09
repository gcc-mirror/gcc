/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-sse4.1 -msse2" } */

void f(double*d,double*e){
  for(;d<e;++d)
    *d=(*d<.5)?.7:0;
}

/* { dg-final { scan-assembler {(?n)(?:cmpnltsd|cmpltsd)} } } */
/* { dg-final { scan-assembler {(?n)(?:andnpd|andpd)} } } */
