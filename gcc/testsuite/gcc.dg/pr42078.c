/* PR tree-optimization/42078 */
/* { dg-do compile } */
/* { dg-options "-g -O -ffast-math" } */

double sqrt (double x);

float
foo (float x)
{
  float y = sqrt (x);
  return x / y;
}

inline float
bar (float x)
{
  float y = sqrt (x);
  float a = y;
  float b = y;
  float c = y;
  return x / y;
}
