/* { dg-additional-options "-std=gnu89" } */

int
foo (x, c)
     int x;
{
  return x >> 24 & 0xff;
}

bar (x)
{
  return (int)(x & 0xfffff) << 13;
}
