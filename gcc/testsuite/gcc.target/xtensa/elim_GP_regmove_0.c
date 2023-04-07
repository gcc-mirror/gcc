/* { dg-do compile } */
/* { dg-options "-O2 -fpeephole2" } */

/* can be processed */
double test0(double a, double b) {
  return __builtin_copysign(a, b);
}

/* cannot be processed: due to violate '0' constraint of the 2nd source operand.  */
int test1(int a, int b) {
  int c;
  asm volatile ("" : "=a"(c) : "r"(a), "0"(b));
  return c;
}

/* cannot be processed: due to violate '&' constraint of the destination operand.  */
int test2(int a) {
  int b;
  asm volatile ("" : "=&a"(b) : "r"(a));
  return b;
}

/* { dg-final { scan-assembler-times "mov\t|mov.n\t" 2 } } */
