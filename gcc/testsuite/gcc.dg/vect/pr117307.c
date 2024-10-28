/* { dg-do compile } */
/* { dg-additional-options "-march=x86-64-v4" { target { x86_64-*-* i?86-*-* } } } */

int a;
float *b, *c;
float d;
void e() {
  for (; a; a++) {
    if (d) {
      c[0] = b[0];
      c[1] = b[1];
    } else if (b[1])
      c[0] = b[0] * 0;
    b += 2;
    c += 2;
  }
}
