/* { dg-do run } */

typedef unsigned int int_t;

__attribute__ ((noinline)) int_t
movigtu (int_t w, int_t x, int_t y, int_t z)
{
  return w > x ? y : z;
}

int
main (void)
{
  if (movigtu (-1, -1, 12, 23) != 23)
    return 1;
  if (movigtu (-1, 3, 12, 23) != 12)
    return 1;
  if (movigtu (1, 3, 12, 23) != 23)
    return 1;
  if (movigtu (3, 3, 12, 23) != 23)
    return 1;
  if (movigtu (5, 3, 12, 23) != 12)
    return 1;
  if (movigtu (3, -1, 12, 23) != 23)
    return 1;
  if (movigtu (3, 1, 12, 23) != 12)
    return 1;
  if (movigtu (3, 5, 12, 23) != 23)
    return 1;
  return 0;
}
