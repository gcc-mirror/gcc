/* { dg-do compile } */
/* { dg-options "-O3" } */

int printf (const char *, ...);

int a, c[1], d, e, **f;

void fn1 (int h)
{
  int *i = 0;
  for (d = 0; d < 1; d++)
    {
      if (d)
        continue;
      for (; e; e++)
        {
          a = c[*i];
          if (h)
            printf ("0");
        }
      return;
    }
  f = &i;
}

