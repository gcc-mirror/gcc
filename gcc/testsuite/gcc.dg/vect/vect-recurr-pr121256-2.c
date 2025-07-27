/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include "tree-vect.h"

#define B 0
#define G 1
#define R 2

int red = 153;
int green = 66;
int blue = 187;

static void __attribute__((noipa))
sub_left_prediction_bgr32(int *restrict dst, int *restrict src)
{
  for (int i = 0; i < 8; i++) {
    int rt = src[i * 3 + R];
    int gt = src[i * 3 + G];
    int bt = src[i * 3 + B];

    dst[i * 3 + R] = rt - red;
    dst[i * 3 + G] = gt - green;
    dst[i * 3 + B] = bt - blue;

    red = rt;
    green = gt;
    blue = bt;
  }
}

int main()
{
  int dst[8*3];
  int src[8*3] = { 160, 73, 194, 17, 33, 99, 0, 12, 283, 87, 73, 11,
		   9, 7, 1, 23, 19, 13, 77, 233, 97, 78, 2, 5 };
  int dst2[8*3] = {-27, 7, 41, -143, -40, -95, -17, -21, 184, 87, 61,
      -272, -78, -66, -10, 14, 12, 12, 54, 214, 84, 1, -231, -92};

  check_vect ();

  sub_left_prediction_bgr32(dst, src);

#pragma GCC novector
  for (int i = 0; i < 8*3; ++i)
    if (dst[i] != dst2[i])
      __builtin_abort();

  return 0;
}
