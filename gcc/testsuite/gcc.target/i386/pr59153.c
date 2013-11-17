/* { dg-do compile } */
/* { dg-options "-O -flive-range-shrinkage -mdispatch-scheduler -march=bdver1" } */

int foo (float f)
{
  union
  {
    float f;
    int i;
  } z = { .f = f };

  return z.i - 1;
}
