/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned a;
int b, c, d, e;
void f() {
  b = 5;
  for (; b <= 51; b++)
    ;
  unsigned int g = -8;
  while (g) {
    g += 5;
    int h = 10;
    do {
      h -= a = 1;
      for (; a; a++)
        ;
      c *= c >= d >= b;
    } while (h);
    c -= e;
  }
}
