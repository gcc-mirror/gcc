/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-additional-options "-O3 -march=armv9.2-a+sve --std=c99" { target { aarch64*-*-* } } } */

void foo(double * restrict a, double * restrict b, int n){
  int i;
  for(i=0; i<n; ++i){
    a[i] = a[i] + b[i];
    __builtin_prefetch(&(b[i+8]));
  }
}

/* { dg-final { scan-assembler-not "prfm" } } */
/* { dg-final { scan-assembler "fadd\tz\[0-9\]+.d, p\[0-9\]+/m, z\[0-9\]+.d, z\[0-9\]+.d" } } */
