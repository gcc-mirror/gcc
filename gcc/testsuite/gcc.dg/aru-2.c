/* { dg-do run } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-O2 -pg" } */

static int __attribute__((noinline))
bar (int x)
{
  return x + 3;
}

int __attribute__((noinline))
foo (int y0, int y1, int y2, int y3, int y4) {
  int r = 0;
  r += bar (r + y4);
  r += bar (r + y3);
  r += bar (r + y2);
  r += bar (r + y1);
  r += bar (r + y0);
  return r;
}

int
main (void)
{
  int z = foo (0, 1, 2, 3, 4);
  return !(z == 191);
}

/* { dg-final { cleanup-profile-file } } */
