/* { dg-additional-options "-std=gnu89" } */

f (c)
    unsigned char c;
{
  if (c != 0xFF)
    abort ();
}

main ()
{
  f (-1);
  exit (0);
}
