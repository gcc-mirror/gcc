/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  int bar = 0;

  return (unsigned) (a - 1) <= (unsigned) bar;
}

main ()
{
  if (foo (-1))
    puts ("The largest possible unsigned <= 0 on this machine...");
}
