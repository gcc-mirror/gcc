/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
#include "tree-vect.h"

extern float copysignf (float, float);
extern float sqrtf (float);
extern float fabsf (float);
extern void abort (void);
float a[64], b[64], c[64], d[64];

__attribute__((noinline, noclone)) void
f1 (int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[4 * i + 0] = copysignf (b[4 * i + 0], c[4 * i + 0]) + 1.0f + sqrtf (d[4 * i + 0]);
      a[4 * i + 1] = copysignf (b[4 * i + 1], c[4 * i + 1]) + 2.0f + sqrtf (d[4 * i + 1]);
      a[4 * i + 2] = copysignf (b[4 * i + 2], c[4 * i + 2]) + 3.0f + sqrtf (d[4 * i + 2]);
      a[4 * i + 3] = copysignf (b[4 * i + 3], c[4 * i + 3]) + 4.0f + sqrtf (d[4 * i + 3]);
    }
}

__attribute__((noinline, noclone)) void
f2 (int n)
{
  int i;
  for (i = 0; i < 2 * n; i++)
    {
      a[2 * i + 0] = copysignf (b[2 * i + 0], c[2 * i + 0]) + 1.0f + sqrtf (d[2 * i + 0]);
      a[2 * i + 1] = copysignf (b[2 * i + 1], c[2 * i + 1]) + 2.0f + sqrtf (d[2 * i + 1]);
    }
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int i;
  for (i = 0; i < 64; i++)
    a[i] = copysignf (b[i], c[i]) + 1.0f + sqrtf (d[i]);
}

__attribute__((noinline, noclone)) void
f4 (int n)
{
  int i;
  for (i = 0; i < 2 * n; i++)
    {
      a[3 * i + 0] = copysignf (b[3 * i + 0], c[3 * i + 0]) + 1.0f + sqrtf (d[3 * i + 0]);
      a[3 * i + 1] = copysignf (b[3 * i + 1], c[3 * i + 1]) + 2.0f + sqrtf (d[3 * i + 1]);
      a[3 * i + 2] = copysignf (b[3 * i + 2], c[3 * i + 2]) + 3.0f + sqrtf (d[3 * i + 2]);
    }
}

__attribute__((noinline, noclone)) int
main1 ()
{
  int i;

  for (i = 0; i < 64; i++)
    {
      asm ("");
      b[i] = (i & 1) ? -4 * i : 4 * i;
      c[i] = (i & 2) ? -8 * i : 8 * i;
      d[i] = i * i;
    }
  f1 (16);
  for (i = 0; i < 64; i++)
    if (fabsf (((i & 2) ? -4 * i : 4 * i) + 1 + (i & 3) + i - a[i]) >= 0.0001f)
      abort ();
    else
      a[i] = 131.25;
  f2 (16);
  for (i = 0; i < 64; i++)
    if (fabsf (((i & 2) ? -4 * i : 4 * i) + 1 + (i & 1) + i - a[i]) >= 0.0001f)
      abort ();
    else
      a[i] = 131.25;
  f3 ();
  for (i = 0; i < 64; i++)
    if (fabsf (((i & 2) ? -4 * i : 4 * i) + 1 + i - a[i]) >= 0.0001f)
      abort ();
    else
      a[i] = 131.25;
  f4 (10);
  for (i = 0; i < 60; i++)
    if (fabsf (((i & 2) ? -4 * i : 4 * i) + 1 + (i % 3) + i - a[i]) >= 0.0001f)
      abort ();
  return 0;
}

int
main ()
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" { target { vect_call_copysignf && vect_call_sqrtf } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "vect" { target { { vect_call_copysignf && vect_call_sqrtf } && vect_perm3_int } } } } */
