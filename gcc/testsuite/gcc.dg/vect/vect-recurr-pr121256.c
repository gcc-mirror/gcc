/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include "tree-vect.h"

#define B 0
#define G 1
#define R 2
#define A 3

int red = 153;
int green = 66;
int blue = 187;
int alpha = 255;

static void __attribute__((noipa))
sub_left_prediction_bgr32(uint8_t *restrict dst, uint8_t *restrict src, int w)
{
  for (int i = 0; i < 8; i++) {
    int rt = src[i * 4 + R];
    int gt = src[i * 4 + G];
    int bt = src[i * 4 + B];
    int at = src[i * 4 + A];

    dst[i * 4 + R] = rt - red;
    dst[i * 4 + G] = gt - green;
    dst[i * 4 + B] = bt - blue;
    dst[i * 4 + A] = at - alpha;

    red = rt;
    green = gt;
    blue = bt;
    alpha = at;
  }
}

int main()
{
  check_vect ();

  uint8_t *dst = calloc(36, sizeof(uint8_t));
  uint8_t *src = calloc(36, sizeof(uint8_t));

  src[R] = 160;
  src[G] = 73;
  src[B] = 194;
  src[A] = 255;

  sub_left_prediction_bgr32(dst, src, 33);
  if (dst[R] != 7 || dst[B] != 7 || dst[A] != 0)
    __builtin_abort();
}
