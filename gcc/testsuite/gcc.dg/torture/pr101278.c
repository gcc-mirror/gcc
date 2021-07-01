/* { dg-do run } */

struct X { int counter; };

struct X __attribute__((noipa)) foo (struct X x)
{
  x.counter++;
  if (x.counter == 5)
    __builtin_exit (0);
  return x;
}

int
main ()
{
  struct X x;
  x.counter = 0;
  for (int i = 0; i < 10; ++i)
    x = foo (x);
  __builtin_abort ();
}
