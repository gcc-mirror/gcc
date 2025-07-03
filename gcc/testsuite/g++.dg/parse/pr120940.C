// PR c++/120940
// { dg-do run }

int a[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
int b[8] = { 9, 10, 11, 12, 13, 14, 15, 16 };

__attribute__((noipa)) int
foo (int x, int y)
{
  return (x ? a : b)[y];
}

int
main ()
{
  if (foo (1, 4) != 5 || foo (0, 6) != 15)
    __builtin_abort ();
}
