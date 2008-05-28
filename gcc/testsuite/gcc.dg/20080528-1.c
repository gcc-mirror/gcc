/* { dg-do compile } */
/* { dg-options "-O2 -fsee" } */

unsigned long g(int a, int b) {
  return a / b;
}
unsigned long f(long int a) {
  return g(a, 1<<13);
}
