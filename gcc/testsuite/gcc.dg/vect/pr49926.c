/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a, b, c[10];

void
foo (unsigned int x, int y, int z, int *w)
{
  do
    {
      *w = z;
      y = x;
      if (y)
        for (b = -4; b; b++)
          {
            z = y &= a &= 1;
            y &= c[b + 4];
          }
    }
  while (1);
}

