/* { dg-do compile } */
/* { dg-options "-w" } */

static inline int
foo (float f)
{
  return *((int *) &f) - 1;
}

float
bar (float x, float y, float *z)
{
  float c = y < 0.002f ? 0.002f : y;
  float d = x < c ? c : x;
  return z[foo (c)] + z[foo (d * 255.0f)];
}
