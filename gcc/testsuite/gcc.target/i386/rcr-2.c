/* { dg-do compile { target ia32 } } */
/* { dg-options "-Oz -mno-stv" } */
unsigned long long foo(unsigned long long x) { return x >> 1; }
long long bar(long long x) { return x >> 1; }
/* { dg-final { scan-assembler-times "rcrl" 2 } } */
/* { dg-final { scan-assembler-not "shrdl" } } */
