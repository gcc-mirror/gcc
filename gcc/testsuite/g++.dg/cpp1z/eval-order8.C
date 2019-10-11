// PR c++/91987
// { dg-do run }
// { dg-options "-fstrong-eval-order" }

int a[4] = { 1, 2, 3, 4 };
int b;

int
main ()
{
  int *x = a;
  b = 1;
  int r = (b = 4, x)[(b *= 2, 3)];
  if (b != 8 || r != 4)
    __builtin_abort ();
  b = 1;
  r = (b = 3, 2)[(b *= 2, x)];
  if (b != 6 || r != 3)
    __builtin_abort ();
}
