/* { dg-do run } */
/* { dg-options "-O2 -march=armv7-a" } */

volatile int mem;

int
bar (int x, int y)
{
  if (x)
    __sync_fetch_and_add(&mem, y);
  return 0;
}

extern void abort (void);

int
main (int argc, char *argv[])
{
  mem = 0;
  bar (0, 1);
  bar (1, 1);
  if (mem != 1)
    abort ();
  return 0;
}
