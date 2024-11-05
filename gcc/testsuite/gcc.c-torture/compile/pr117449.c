/* { dg-additional-options "-march=armv8.2-a+sha3" { target aarch64*-*-* } } */

unsigned long *a;
int i;
void f() {
  for (i = 0; i < 80; i++)
    a[i] = (a[i] >> 8 | a[i] << 64 - 8) ^ a[i];
}
