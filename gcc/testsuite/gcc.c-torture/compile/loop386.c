/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  do
    {
      puts ("a");
      a -= 1;
    }
  while (a != 0);
}

main ()
{
  int p[100];
  printf ("%d\n", foo (3));
}
