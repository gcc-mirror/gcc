/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize" } */
/* { dg-additional-options "-march=armv8.2-a+sve" { target aarch64*-*-* } } */

int a, b, c;

int g() {
  char i = 0;
  for (c = 0; c <= 8; c++)
    --i;

  while (b) {
    _Bool f = i <= 0;
    a = (a == 0) ? 0 : f / a;
  }
}
