/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -mno-autovec-segment" } */

typedef struct {
  int A[6];
  float b[];
} a;

int b(a *a) {
  int b = 0;
  for (; b < 3; b++) {
    a->A[2 * b] = a->b[b] - b + a->A[2 * b];
    a->A[2 * b + 1] = b * a->A[2 * b + 1];
  }
  return 0;
}

/* { dg-final { scan-assembler-not "vsetivli.*zero,0" } } */
