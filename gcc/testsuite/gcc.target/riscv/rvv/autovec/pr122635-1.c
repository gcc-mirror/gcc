/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -mno-autovec-segment" } */

typedef struct {
  int a[6];
  float b[3];
} c;

int d(c *e) {
  int f =0;
  for (; f < 3; f++) {
    e->a[2 * f] = e->b[f];
    e->a[2 * f + 1] = -e->a[2 * f];
    e->a[2 * f] = f + 3 * e->a[2 * f];
    e->a[2 * f + 1] = f + 3 * e->a[2 * f + 1];
  }
  return 0;
}

/* { dg-final { scan-assembler-not "vsetivli.*zero,0" } } */
