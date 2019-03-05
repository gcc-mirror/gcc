/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "copysign_1.c"

extern void abort ();

#define N 16
_Float16 ah[N] = { -0.1f16, -3.2f16, -6.3f16, -9.4f16,
  -12.5f16, -15.6f16, -18.7f16, -21.8f16,
  24.9f16, 27.1f16, 30.2f16, 33.3f16,
  36.4f16, 39.5f16, 42.6f16, 45.7f
};

_Float16 bh[N] = { -1.2f16, 3.4f16, -5.6f16, 7.8f16,
  -9.0f16, 1.0f16, -2.0f16, 3.0f16,
  -4.0f16, -5.0f16, 6.0f16, 7.0f16,
  -8.0f16, -9.0f16, 10.0f16, 11.0f16
};

_Float16 rh[N];

float a[N] = { -0.1f, -3.2f, -6.3f, -9.4f,
  -12.5f, -15.6f, -18.7f, -21.8f,
  24.9f, 27.1f, 30.2f, 33.3f,
  36.4f, 39.5f, 42.6f, 45.7f
};

float b[N] = { -1.2f, 3.4f, -5.6f, 7.8f,
  -9.0f, 1.0f, -2.0f, 3.0f,
  -4.0f, -5.0f, 6.0f, 7.0f,
  -8.0f, -9.0f, 10.0f, 11.0f
};

float r[N];

double ad[N] = { -0.1d, -3.2d, -6.3d, -9.4d,
  -12.5d, -15.6d, -18.7d, -21.8d,
  24.9d, 27.1d, 30.2d, 33.3d,
  36.4d, 39.5d, 42.6d, 45.7d
};

double bd[N] = { -1.2d, 3.4d, -5.6d, 7.8d,
  -9.0d, 1.0d, -2.0d, 3.0d,
  -4.0d, -5.0d, 6.0d, 7.0d,
  -8.0d, -9.0d, 10.0d, 11.0d
};

double rd[N];

int
main (void)
{
  int i;

  copysign_half (ah, bh, rh, N);
  for (i = 0; i < N; i++)
    if (rh[i] != __builtin_copysignf16 (ah[i], bh[i]))
      abort ();

  copysign_float (a, b, r, N);
  for (i = 0; i < N; i++)
    if (r[i] != __builtin_copysignf (a[i], b[i]))
      abort ();

  copysign_double (ad, bd, rd, N);
  for (i = 0; i < N; i++)
    if (rd[i] != __builtin_copysign (ad[i], bd[i]))
      abort ();

  return 0;
}
