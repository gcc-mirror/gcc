/* { dg-do compile } */
/* { dg-additional-options "-Ofast -mcpu=neoverse-v2" } */

int a;
float *b;

void foo() {
  for (; a; a--, b += 4) {
    b[0] = b[1] = b[2] = b[2] > 0 ?: 0;
    if (b[3] < 0)
      b[3] = 0;
  }
}
