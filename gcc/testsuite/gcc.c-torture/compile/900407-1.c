/* { dg-additional-options "-std=gnu89" } */

foo (a, b, p)
     int *p;
{
  int c;
  p[1] = a + 0x1000;
  c = b + 0xffff0000;
  if ((b + 0xffff0000) == 0)
    c++;
  p[2] = c;
}
