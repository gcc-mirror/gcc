// PR c++/91987
// { dg-do run }
// { dg-options "-fstrong-eval-order" }

int
foo ()
{
  int x = 5;
  int r = x << (x = 3, 2);
  if (x != 3)
    __builtin_abort ();
  return r;
}

int
main ()
{
  if (foo () != (5 << 2))
    __builtin_abort ();
}
