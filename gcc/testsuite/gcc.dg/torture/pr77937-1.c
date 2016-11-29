/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=amdfam10" { target i?86-*-* x86_64-*-* } } */

int *a;
int b, c, d;
void fn1(char *p1, int p2) {
  int x;
  while (1) {
    x = 0;
    for (; x < 8; x++)
      p1[0] = -a[0] * d + p1[0] * c + 1 >> b >> 1;
    p1 += p2;
  }
}
