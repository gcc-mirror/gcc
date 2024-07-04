/* { dg-do run } */

typedef int int_t;

__attribute__ ((noinline)) int_t
addifeq (double w, double x, int_t y, int_t z)
{
  return w == x ? y + z : y;
}

int
main (void)
{
  if (addifeq (-1.0, -1.0, 12, 23) != 35)
    return 1;
  if (addifeq (-1.0, 3.0, 12, 23) != 12)
    return 1;
  if (addifeq (1.0, 3.0, 12, 23) != 12)
    return 1;
  if (addifeq (3.0, 3.0, 12, 23) != 35)
    return 1;
  if (addifeq (5.0, 3.0, 12, 23) != 12)
    return 1;
  if (addifeq (3.0, -1.0, 12, 23) != 12)
    return 1;
  if (addifeq (3.0, 1.0, 12, 23) != 12)
    return 1;
  if (addifeq (3.0, 5.0, 12, 23) != 12)
    return 1;
  return 0;
}
