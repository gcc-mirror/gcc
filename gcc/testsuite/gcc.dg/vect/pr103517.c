/* { dg-do compile } */
/* { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } } */

int a;
short b, c;
extern short d[];
void e() {
  for (short f = 1; f < (short)a; f += 2)
    if (d[f + 1]) {
      b = d[f];
      c = d[f + 1];
    }
}
