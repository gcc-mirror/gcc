/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=armv9-a" { target aarch64*-*-* } } */
/* { dg-additional-options "-march=skylake-avx512" { target x86_64*-*-* } } */

extern char a[];
extern short b[], c[];
extern int h[];
void i() {
  for (char d;;)
    for (int e = 0; e < 9; e++) {
      for (int f; f < 9; f += 4)
        c[d] = 0;
      for (int g = 0; g < 9; g += 3)
        h[1 + d] = (a[d] ? a[3] : 7 ? b[7] : 0) / 6;
    }
}
