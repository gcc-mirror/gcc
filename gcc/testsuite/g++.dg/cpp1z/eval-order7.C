// PR c++/91987
// { dg-do run }
// { dg-options "-fstrong-eval-order" }

int a[4] = { 1, 2, 3, 4 };
int b[4] = { 5, 6, 7, 8 };

int
foo ()
{
  int *x = a;
  int r = x[(x = b, 3)];
  if (x != b)
    __builtin_abort ();
  return r;
}

int
main ()
{
  if (foo () != 4)
    __builtin_abort ();
}
