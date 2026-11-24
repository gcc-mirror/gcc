/* PR tree-optimization/112628 */
/* { dg-do run } */
/* { dg-additional-options "-fwrapv" } */

/* Check the fold is correct when signed overflow wraps:
   then ~A < -A is false for A == INT_MIN.  */

__attribute__((noipa)) int
slt (int a)
{
  return (~a) < -a;
}

__attribute__((noipa)) int
seq (int a)
{
  return (~a) == -a;
}

__attribute__((noipa)) int
ult (unsigned a)
{
  return (~a) < -a;
}

__attribute__((noipa)) int
uge (unsigned a)
{
  return (~a) >= -a;
}

int
main ()
{
  int sv[] = { 0, 1, -1, 42, -42, __INT_MAX__, -__INT_MAX__ - 1 };
  unsigned uv[] = { 0, 1, 2, 42, 0x80000000u, ~0u };
  unsigned i;

  for (i = 0; i < sizeof (sv) / sizeof (sv[0]); i++)
    {
      int a = sv[i];
      if (slt (a) != (a != -__INT_MAX__ - 1))
	__builtin_abort ();
      if (seq (a) != 0)
	__builtin_abort ();
    }

  for (i = 0; i < sizeof (uv) / sizeof (uv[0]); i++)
    {
      unsigned a = uv[i];
      if (ult (a) != (a != 0))
	__builtin_abort ();
      if (uge (a) != (a == 0))
	__builtin_abort ();
    }

  return 0;
}
