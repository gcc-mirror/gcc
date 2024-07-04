/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  a++;
  if (a < 10)
    return 1;
  return a;
}

main ()
{
  printf ("%d\n", foo ((1 << 31) - 1));
}
