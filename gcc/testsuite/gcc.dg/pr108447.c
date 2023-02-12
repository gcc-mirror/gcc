/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
foo (float x, float y)
{
  _Bool cmp1 = x <= y;
  _Bool cmp2 = x >= y;
  if (cmp1 && cmp2)
    return 1;
  else if (!cmp1 && !cmp2)
    return -1;
  return 0;
}

int
main ()
{
  if (foo (0.0f, __builtin_nanf ("")) != -1)
    __builtin_abort ();
  if (foo (__builtin_nanf (""), -42.0f) != -1)
    __builtin_abort ();
  if (foo (0.0f, -0.0f) != 1)
    __builtin_abort ();
  if (foo (42.0f, 42.0f) != 1)
    __builtin_abort ();
  if (foo (42.0f, -0.0f) != 0)
    __builtin_abort ();
  if (foo (0.0f, -42.0f) != 0)
    __builtin_abort ();
  return 0;
}

