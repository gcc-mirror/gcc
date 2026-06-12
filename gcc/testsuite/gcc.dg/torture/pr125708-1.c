/* PR middle-end/125708 */
/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

__attribute__((noipa)) int
foo (int a, _Bool b)
{
  return a / (2 - b);
}

__attribute__((noipa)) int
foo1 (int a, _Bool b)
{
  return a / (4 + b);
}

__attribute__((noipa)) int
foo2 (int a, _Bool b)
{
  return a / (8 - b);
}

__attribute__((noipa)) unsigned
foo3 (unsigned a, _Bool b)
{
  return a / (4 + b);
}

__attribute__((noipa)) int
foo4 (int a, _Bool b)
{
  return a / ((1 << 20) - b);
}

int
main (void)
{
  static const int vals[] =
    { 0, 1, 2, 3, 7, 8, 15, 16, 100, -1, -7, -8, -100,
      1000000, -1000000, __INT_MAX__, -__INT_MAX__ - 1 };

  for (unsigned i = 0; i < sizeof (vals) / sizeof (vals[0]); i++)
    {
      int a = vals[i];
      for (int b = 0; b <= 1; b++)
	{
	  if (foo (a, b) != a / (2 - b))
	    __builtin_abort ();
	  if (foo1 (a, b) != a / (4 + b))
	    __builtin_abort ();
	  if (foo2 (a, b) != a / (8 - b))
	    __builtin_abort ();
	  if (foo3 ((unsigned) a, b) != (unsigned) a / (4u + b))
	    __builtin_abort ();
	  if (foo4 (a, b) != a / ((1 << 20) - b))
	    __builtin_abort ();
	}
    }
  return 0;
}
