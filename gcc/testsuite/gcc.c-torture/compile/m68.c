/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  return a | 12345;
}

bar (a)
{
  return a & (0xffff0000 | 12345);
}

foobar (a)
{
  return a - 128;
}
