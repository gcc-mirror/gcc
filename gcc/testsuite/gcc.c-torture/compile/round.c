/* { dg-additional-options "-std=gnu89" } */

foo (a)
     double a;
{
  printf ("%d\n", (int) a);
}

main ()
{
  foo (1.6);
  foo (1.4);
  foo (-1.4);
  foo (-1.6);
}
