/* { dg-do compile } */

int a, b, c, *d, *f[1];

void fn1 (int *j)
{
  int e, g, h = 1;
  for (; e; e++)
    {
      if (g > 0)
        {
          d = j;
          return;
        }
      if (!h)
        while (g)
          ;
      while (h < 1)
        if (a)
          {
            fn1 (&h);
            h = 0;
          }
      f[e] = &c;
    }
  while (1)
    ;
}
