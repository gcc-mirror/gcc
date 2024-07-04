/* { dg-additional-options "-std=gnu89" } */

long long
foo (a)
     int a;
{
  return a;
}

main ()
{
  printf ("%d\n", (int) (foo (-1) >> 32));
}
