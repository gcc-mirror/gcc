/* PR tree-optimization/120638 */
/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern float sqrtf (float x);

__attribute__((noipa)) float
foo (unsigned int s)
{
  return 0.5f / sqrtf (1.f + s);
}

__attribute__((noipa)) float
bar (float s)
{
  if (s < 0.0 || s > 65535.0f)
    __builtin_unreachable ();
  return 0.5f / sqrtf (1.f + s);
}

int
main ()
{
  if (__builtin_fabsf (foo (3) - 0.25f) > 0.00390625f
      || __builtin_fabsf (foo (15) - 0.125f) > 0.00390625f
      || __builtin_fabsf (foo (63) - 0.0625f) > 0.00390625f
      || __builtin_fabsf (bar (3.0f) - 0.25f) > 0.00390625f
      || __builtin_fabsf (bar (15.0f) - 0.125f) > 0.00390625f
      || __builtin_fabsf (bar (63.0f) - 0.0625f) > 0.00390625f)
    __builtin_abort ();
}
