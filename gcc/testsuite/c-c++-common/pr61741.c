/* { dg-do run } */

int a = 1, b;

void
foo (void)
{
  signed char c = 0;
  for (; a; a--)
    for (; c >= 0; c++);
  if (!c)
    b = 1;
}

int
main ()
{
  foo ();
  if (b != 0)
    __builtin_abort ();
  return 0;
}
