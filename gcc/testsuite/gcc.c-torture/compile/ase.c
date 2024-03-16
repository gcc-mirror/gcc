/* { dg-additional-options "-std=gnu89" } */

ase (p)
     short *p;
{
  int a;
  a = p[1];
  p[2] = a;
  if ((short) a)
    p[a]++;
  return (a == 0);
}
