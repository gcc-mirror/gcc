/* { dg-additional-options "-std=gnu89" } */

foo (p)
     int *p;
{
  int x;
  int a;

  a = p[0];
  x = a + 5;
  a = -1;
  p[0] = x - 5;
  return a;
}

bar (p)
{
  short x;
  int a;

  x = ((short *) p)[1];
#if INHIBIT_COMBINE
  ((short *) p)[0] = x;
#endif

  return (x < 45);
}
