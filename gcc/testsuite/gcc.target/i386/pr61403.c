/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse4.2 -mtune=corei7" } */
/* { dg-require-effective-target c99_runtime } */

#include <math.h>

struct XYZ
{
  float x;
  float y;
  float z;
};

void
norm (struct XYZ *in, struct XYZ *out, int size)
{
  int i;
  for (i = 0; i < size; ++i)
    {
      float n = sqrt (in[i].x * in[i].x + in[i].y * in[i].y + in[i].z * in[i].z);
      out[i].x = in[i].x / n;
      out[i].y = in[i].y / n;
      out[i].z = in[i].z / n;
    }
}

/* { dg-final { scan-assembler "rsqrtps" } } */
