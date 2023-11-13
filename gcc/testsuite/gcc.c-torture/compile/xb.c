/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
{
  unsigned x = 1;

  a += b;
  a += x;
  if (a <= 0)
    return 1;
  return 0;
}

main ()
{
  printf ("%d\n", foo (1, ~0));
  printf ("%d\n", foo (0, ~0));
  printf ("%d\n", foo (-1, ~0));
}
