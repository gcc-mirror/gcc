/* { dg-additional-options "-std=gnu89" } */

long long
foo (a, b)
     long long a, b;
{
  return a * b;
}

main ()
{
  int a = foo ((long long) 2, (long long) 3);
  printf ("%d\n", a);
}
