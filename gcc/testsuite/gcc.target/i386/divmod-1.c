/* { dg-do run } */
/* { dg-options "-O2 -m8bit-idiv" } */

extern void abort (void);

void
__attribute__((noinline))
test (int x, int y, int q, int r)
{
  if ((x / y) != q || (x % y) != r)
    abort ();
}

int
main ()
{
  test (7, 6, 1, 1);
  test (-7, -6, 1, -1);
  test (-7, 6, -1, -1);
  test (7, -6, -1, 1);
  test (255, 254, 1, 1);
  test (256, 254, 1, 2);
  test (256, 256, 1, 0);
  test (254, 256, 0, 254);
  test (254, 255, 0, 254);
  test (254, 1, 254, 0);
  test (255, 2, 127, 1);
  test (1, 256, 0, 1);
  return 0;
}
