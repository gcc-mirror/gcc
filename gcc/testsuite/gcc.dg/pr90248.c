/* PR tree-optimization/90248 */
/* { dg-do run } */
/* { dg-options "-Ofast" } */

volatile float b1 = -1.f;
volatile float b2 = 0.f;

__attribute__((noipa)) float
f1 (float x)
{
  return x > 0 ? 1.f : -1.f;
}

__attribute__((noipa)) float
f2 (float x)
{
  return x >= 0 ? 1.f : -1.f;
}

__attribute__((noipa)) float
f3 (float x)
{
  return x < 0 ? 1.f : -1.f;
}

__attribute__((noipa)) float
f4 (float x)
{
  return x <= 0 ? 1.f : -1.f;
}

__attribute__((noipa)) float
f5 (float x)
{
  return x > 0 ? -1.f : 1.f;
}

__attribute__((noipa)) float
f6 (float x)
{
  return x >= 0 ? -1.f : 1.f;
}

__attribute__((noipa)) float
f7 (float x)
{
  return x < 0 ? -1.f : 1.f;
}

__attribute__((noipa)) float
f8 (float x)
{
  return x <= 0 ? -1.f : 1.f;
}

int
main ()
{
  float a = 0.f;
  float b = b1 * b2;
  float c = 2.f;
  float d = -2.f;
  if (f1 (a) != -1.f || f1 (b) != -1.f || f1 (c) != 1.f || f1 (d) != -1.f
      || f2 (a) != 1.f || f2 (b) != 1.f || f2 (c) != 1.f || f2 (d) != -1.f
      || f3 (a) != -1.f || f3 (b) != -1.f || f3 (c) != -1.f || f3 (d) != 1.f
      || f4 (a) != 1.f || f4 (b) != 1.f || f4 (c) != -1.f || f4 (d) != 1.f
      || f5 (a) != 1.f || f5 (b) != 1.f || f5 (c) != -1.f || f5 (d) != 1.f
      || f6 (a) != -1.f || f6 (b) != -1.f || f6 (c) != -1.f || f6 (d) != 1.f
      || f7 (a) != 1.f || f7 (b) != 1.f || f7 (c) != 1.f || f7 (d) != -1.f
      || f8 (a) != -1.f || f8 (b) != -1.f || f8 (c) != 1.f || f8 (d) != -1.f)
    __builtin_abort ();
  return 0;
}
