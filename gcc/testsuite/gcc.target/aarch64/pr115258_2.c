/* { dg-do compile } */
/* { dg-options "-Ofast -mcpu=neoverse-v2" } */

extern __attribute__((aligned(64))) float a[32000], b[32000];
int dummy(float[32000], float[32000], float);

void s1112() {

  for (int nl = 0; nl < 100000 * 3; nl++) {
    for (int i = 32000 - 1; i >= 0; i--) {
      a[i] = b[i] + (float)1.;
    }
    dummy(a, b, 0.);
  }
}

/* { dg-final { scan-assembler-not {\tmov\tv[0-9]+\.16b,} } } */
