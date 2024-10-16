/* { dg-additional-options "-std=gnu17" } */

#include <stdio.h>
#include <stdint.h>

int32_t a[6];
int64_t b;
int32_t *c;
int32_t **d = &c;
int64_t *e = &b;
int32_t **const *f = &d;
int32_t **const **g = &f;
int32_t *h();
static int16_t j();
static uint32_t k(int8_t, const int32_t *, int64_t);
static uint32_t l() {
  int32_t *m = &a[3];
  int32_t n = 0;
  int8_t o = 0;
  int32_t *p[] = {&n, &n, &n, &n};
  uint32_t q[6][1][2] = {};
  for (o = 0; o <= 1; o = 6)
    if (h(j(k(3, 0, q[2][0][0]), &n), n) == p[3])
      *m = *e;
  return 0;
}
int32_t *h(uint32_t, int32_t) { return ***g; }
int16_t j(uint32_t, int32_t *r) { **f = r; return 0;}
uint32_t k(int8_t, const int32_t *, int64_t) { *e = 3; return 0;}
int main() {
  int i = 0;
  l();
  for (i = 0; i < 6; i++){
    if (i == 3 && a[i] != 3)
	__builtin_abort ();
  }
  return 0;
}
