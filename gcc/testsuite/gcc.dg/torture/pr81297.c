/* { dg-do compile } */

int
nf (int gy, int x0)
{
  while (gy < 1)
    ++x0;

  gy += !!gy;
  if (gy < 0)
    {
      x0 += gy;
      return (x0 > (gy + x0)) ? (1 / 0) : 1; /* { dg-warning "division by zero" } */
    }
}

