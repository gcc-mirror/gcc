/* { dg-do run } */

char x[] = {
   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
};

__attribute__ ((noinline)) char *
p (char *a, int o, int i)
{
  return a + ++o + (1 << ++i);
}

int
main (void)
{
  if (*p (x, 0, 0) != 3)
    return 1;
  if (*p (x, 1, 2) != 10)
    return 1;
  if (*p (x, 2, 1) != 7)
    return 1;
  if (*p (x, 3, 3) != 20)
    return 1;
  return 0;
}
