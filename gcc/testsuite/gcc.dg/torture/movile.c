/* { dg-do run } */

typedef int int_t;

__attribute__ ((noinline)) int_t
movile (int_t w, int_t x, int_t y, int_t z)
{
  return w <= x ? y : z;
}

int
main (void)
{
  if (movile (-1, -1, 12, 23) != 12)
    return 1;
  if (movile (-1, 3, 12, 23) != 12)
    return 1;
  if (movile (1, 3, 12, 23) != 12)
    return 1;
  if (movile (3, 3, 12, 23) != 12)
    return 1;
  if (movile (5, 3, 12, 23) != 23)
    return 1;
  if (movile (3, -1, 12, 23) != 23)
    return 1;
  if (movile (3, 1, 12, 23) != 23)
    return 1;
  if (movile (3, 5, 12, 23) != 12)
    return 1;
  return 0;
}
