/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

int
foo (int x, int y)
{
  const int z = 2;
  if (z & 1)
    return x << y;
  return 0;
}

int
bar (int x, int y)
{
  return x + y;
}

int
main (void)
{
  foo (3, 2);
  bar (12, 42);
  return 0;
}
