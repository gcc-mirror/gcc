/* { dg-do run } */

typedef int int_t;

__attribute__ ((noinline)) int_t
movifle (double w, double x, int_t y, int_t z)
{
  return w <= x ? y : z;
}

int
main (void)
{
  if (movifle (-1.0, -1.0, 12, 23) != 12)
    return 1;
  if (movifle (-1.0, 3.0, 12, 23) != 12)
    return 1;
  if (movifle (1.0, 3.0, 12, 23) != 12)
    return 1;
  if (movifle (3.0, 3.0, 12, 23) != 12)
    return 1;
  if (movifle (5.0, 3.0, 12, 23) != 23)
    return 1;
  if (movifle (3.0, -1.0, 12, 23) != 23)
    return 1;
  if (movifle (3.0, 1.0, 12, 23) != 23)
    return 1;
  if (movifle (3.0, 5.0, 12, 23) != 12)
    return 1;
  return 0;
}
