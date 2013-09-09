/* { dg-do compile } */

unsigned f(void)
{
  unsigned a;
  int b, c, d, e;

  for(c = 27; c < 40; c++)
    b |= d |= b;

  if(b)
    a = e;

  return a;
}
