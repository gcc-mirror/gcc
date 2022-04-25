/* { dg-do run } */

__attribute__((noipa)) void
bar (int x)
{
  (void) x;
}

int a;

int
foo (void)
{
  int b, c;
  for (b = 0; b < 3; b++)
    {
      if (!a)
	break;
      c--;
      bar (c);
    }
  return b;
}

int
main ()
{
  if (foo ())
    __builtin_abort ();
  return 0;
}
