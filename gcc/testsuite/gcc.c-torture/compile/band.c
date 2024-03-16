/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  return (a & (1 << 31)) != 0;
}

main ()
{
  if (foo (0))
    puts ("foo");
  else
    puts ("bar");
  if (foo (~0))
    puts ("foo");
  else
    puts ("bar");
}
