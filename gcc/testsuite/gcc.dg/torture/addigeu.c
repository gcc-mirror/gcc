/* { dg-do run } */

typedef unsigned int int_t;

__attribute__ ((noinline)) int_t
addigeu (int_t w, int_t x, int_t y, int_t z)
{
  return w >= x ? y + z : y;
}

int
main (void)
{
  if (addigeu (-1, -1, 12, 23) != 35)
    return 1;
  if (addigeu (-1, 3, 12, 23) != 35)
    return 1;
  if (addigeu (1, 3, 12, 23) != 12)
    return 1;
  if (addigeu (3, 3, 12, 23) != 35)
    return 1;
  if (addigeu (5, 3, 12, 23) != 35)
    return 1;
  if (addigeu (3, -1, 12, 23) != 12)
    return 1;
  if (addigeu (3, 1, 12, 23) != 35)
    return 1;
  if (addigeu (3, 5, 12, 23) != 12)
    return 1;
  return 0;
}
