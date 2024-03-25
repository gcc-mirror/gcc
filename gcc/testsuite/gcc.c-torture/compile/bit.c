/* { dg-additional-options "-std=gnu89" } */

bar (a)
{
  return (a == 0);
}

foo (a)
     int a;
{
  if ((a & (1 << 26)) >= 0)
    return 1;
  else
    return 2;
}
