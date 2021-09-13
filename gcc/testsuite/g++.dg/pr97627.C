/* PR tree-optimization/97627.  */
/* { dg-do run } */

struct S { unsigned short x, y; } m = { 1, 0 };

__attribute__((noipa)) void
baz (int x, int y)
{
  if (x != 0 || y != 1)
    __builtin_abort ();
}

__attribute__((noipa)) void
bar ()
{
  throw 1;
}

void
foo ()
{
  while (1)
    {
      int a = m.x + 1;
      int b = m.y + 1;
      for (int c = 0; c < a; c++)
	for (int d = 0; d < b; d++)
	  baz (d, b);
      bar ();
    }
}

int
main ()
{
  try
    {
      foo ();
    }
  catch (int)
    {
    }
  return 0;
}
