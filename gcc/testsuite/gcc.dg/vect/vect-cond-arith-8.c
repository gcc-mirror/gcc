/* { dg-do compile } */

void
f (float *x, _Bool *cond, float *y)
{
  for (int i = 0; i < 100; ++i)
    x[i] = cond[i] ? y[i] * 100 : y[i];
}
