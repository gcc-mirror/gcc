/* { dg-additional-options "-std=gnu89" } */

foo1 (p)
     char *p;
{
  p[0] = p[1];
  return p[0];
}

foo2 (p, x)
     char *p;
{
  p[0] = x;
  return p[0];
}

