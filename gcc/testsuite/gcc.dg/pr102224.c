/* PR target/102224 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) float
foo (float x)
{
  return x * __builtin_copysignf (1.0f, x);
}

__attribute__((noipa)) float
bar (float x, float y)
{
  return x * __builtin_copysignf (1.0f, y);
}

__attribute__((noipa)) float
baz (float z, float x)
{
  return x * __builtin_copysignf (1.0f, x);
}

__attribute__((noipa)) float
qux (float z, float x, float y)
{
  return x * __builtin_copysignf (1.0f, y);
}

int
main ()
{
  if (foo (1.0f) != 1.0f
      || foo (-4.0f) != 4.0f)
    __builtin_abort ();
  if (bar (1.25f, 7.25f) != 1.25f
      || bar (1.75f, -3.25f) != -1.75f
      || bar (-2.25f, 7.5f) != -2.25f
      || bar (-3.0f, -4.0f) != 3.0f)
    __builtin_abort ();
  if (baz (5.5f, 1.0f) != 1.0f
      || baz (4.25f, -4.0f) != 4.0f)
    __builtin_abort ();
  if (qux (1.0f, 1.25f, 7.25f) != 1.25f
      || qux (2.0f, 1.75f, -3.25f) != -1.75f
      || qux (3.0f, -2.25f, 7.5f) != -2.25f
      || qux (4.0f, -3.0f, -4.0f) != 3.0f)
    __builtin_abort ();
  return 0;
}
