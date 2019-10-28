/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

int a, b;
char c[2];
void d() {
  char e;
  for (; b; b--) {
    e = 0;
    for (; e <= 1; e++)
      a &= c[b + e] && 1;
  }
}
