/* { dg-additional-options "-std=gnu89" } */

f (x, y)
{
  if (x % y != 0)
    abort ();
}

main ()
{
  f (-5, 5);
  exit (0);
}
