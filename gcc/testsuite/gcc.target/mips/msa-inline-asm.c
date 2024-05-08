/* { dg-do compile } */
/* { dg-options "-mno-mips16 -mfp64 -mhard-float -mmsa" } */

double
f(double a, double b, double c) {
  asm volatile ("fmadd.d %w0, %w1, %w2" : "+w"(a): "w"(b), "w"(c));
  return a;
}
/* { dg-final { scan-assembler "fmadd.d \\\$w0, \\\$w\[0-9\]*, \\\$w\[0-9\]*" } }  */
