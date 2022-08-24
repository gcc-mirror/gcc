/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-additional-options "-march=armv8.2-a+sve" { target aarch64-*-* } } */

#define M_PI 3.14f
#define NK 24
#define DIM 3

typedef float rvec_ [3];

static rvec_ v0[NK] = {
      { 1, 0, 0 },  { 0, 1, 0 },  { 0, 0, 1 },  { 1, 1, 0 },  { 1, -1, 0 },
      { 1, 0, 1 },  { 1, 0, -1 }, { 0, 1, 1 },  { 0, 1, -1 }, { 1, 1, 1 },
      { 1, 1, -1 }, { 1, -1, 1 }, { -1, 1, 1 }, { 2, 0, 0 },  { 0, 2, 0 },
      { 0, 0, 2 },  { 3, 0, 0 },  { 0, 3, 0 },  { 0, 0, 3 },  { 4, 0, 0 },
      { 0, 4, 0 },  { 0, 0, 4 } };

static inline float iprod__(const rvec_ a, const rvec_ b)
{
  return (a[0] * b[0] + a[1] * b[1] + a[2] * b[2]);
}

int badaboum(rvec_ cm_mol, float **tc)
{
  float              sx;
  int                k, d;
  rvec_              kfac[3];

  for (k = 0; k < DIM; k++)
    for (d = 0; d < DIM; d++)
      kfac[k][d] = M_PI * v0[k][d] / v0[d][d];

  for (k = 0; k < DIM; k++)
    {
      sx = __builtin_sinf(iprod__(kfac[k], cm_mol));
      tc[k][0] += sx * iprod__(v0[k], cm_mol);
    }

  return 0;
}
