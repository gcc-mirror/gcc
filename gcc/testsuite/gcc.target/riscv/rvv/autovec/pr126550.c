/* { dg-do run } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-additional-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -fno-vect-cost-model -std=gnu99" } */

#include <stdint.h>

struct Pair { uint16_t first, second; };
struct S { uint16_t a[2], b[2]; uint32_t r; };

__attribute__((noipa))
void load(struct S *dst, const struct Pair *src, unsigned long n)
{
  for (unsigned long k = 0; k < n; k++) {
    dst[k].a[0] = src[k].first;
    dst[k].a[1] = src[k].second;
    dst[k].b[0] = src[k].first;
    dst[k].b[1] = src[k].second;
  }
}

#define N 40
static struct Pair src[N];
static struct S    out[N];

int main(void)
{
  for (unsigned i = 0; i < N; i++) {
    src[i].first = (uint16_t)(i * 2u + 1);
    src[i].second = (uint16_t)(i * 3u + 2);
  }
  load(out, src, N);

  for (unsigned i = 0; i < N; i++) {
    uint16_t f = src[i].first, s = src[i].second;
    if (out[i].a[0] != f || out[i].a[1] != s ||
      out[i].b[0] != f || out[i].b[1] != s) {
        __builtin_abort ();
    }
  }
  return 0;
}
