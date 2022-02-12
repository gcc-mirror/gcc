/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=armv8.2-a+sve -msve-vector-bits=128" { target aarch64-*-* } } */

int a, b;
char c;
signed char d(int e, int f) { return e - f; }
void g() {
  a = 0;
  for (; a >= -17; a = d(a, 1))
    c ^= b;
}
