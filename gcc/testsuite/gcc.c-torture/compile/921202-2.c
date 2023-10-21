/* { dg-additional-options "-std=gnu89" } */

f(x, c)
{
  for (;;)
    {
      if (x << c) break;
      x++;
    }
}
