/* { dg-do run } */

int a, b, d;

__attribute__((noipa)) void
foo (void)
{
  ++d;
}

int
main ()
{
  for (a = 0; a > -3; a -= 2)
    {
      int c = a;
      b = __INT_MAX__ - 3000;
      a = ~c * b;
      foo ();
      if (!a)
        break;
      a = c;
    }
  if (d != 2)
    __builtin_abort ();
  return 0;
}
