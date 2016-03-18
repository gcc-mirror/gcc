/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=skylake-avx512" { target { i?86-*-* x86_64-*-* } } } */

extern unsigned char a [150];
extern unsigned char b [150];
extern unsigned char c [150];
extern unsigned char d [150];
extern unsigned char e [150];

void foo () {
  for (int i = 92; i <= 141; i += 2) {
    int tmp = (d [i] && b [i]) <= (a [i] > c [i]);
    e [i] = tmp >> b [i];
  }
}
