/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-times "vmovapd|vmovsd" 2 } } */

static inline double g (double x){
  asm volatile ("" : "+x" (x));
  return x;
}
static inline double f (double a, double b){
  return g (g (a) + g (b));
}
double h (double a, double b){
  return f (f (a, a), f (b, b));
}
