/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mfpmath=sse" } */
float doit(float k){
    float c[2]={0.0};
    c[1]+=k;
    return c[0]+c[1];
}

/* { dg-final { scan-assembler "pxor" } } */
