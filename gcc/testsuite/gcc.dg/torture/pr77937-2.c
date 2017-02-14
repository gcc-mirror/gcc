/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=amdfam10" { target i?86-*-* x86_64-*-* } } */

extern int fn2(int);
extern int fn3(int);
int a, b, c;
void fn1(long p1) {
  char *d;
  for (;; d += p1) {
    d[0] = fn2(1 >> a);
    fn3(0);
    fn3(c >> a);
    d[1] = fn3(d[1] * b + c >> a);
    d[4] = fn3(d[4] * b + c >> a);
    d[5] = fn3(d[5] * b + c >> a);
  }
}
