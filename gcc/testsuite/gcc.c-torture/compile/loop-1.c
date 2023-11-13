/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  while ((a -= 1) != -1)
    bar (270000);
  putchar ('\n');
}

main ()
{
  foo (5);
}
