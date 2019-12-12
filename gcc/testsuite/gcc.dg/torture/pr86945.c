/* { dg-do run } */

void __attribute__((noinline,noipa))
foo(int id)
{
  switch (id)
    {
    case (-__INT_MAX__ - 1)...-1:
      __builtin_abort ();
    default:;
    }
}

int main()
{
  foo(1);
  return 0;
}
