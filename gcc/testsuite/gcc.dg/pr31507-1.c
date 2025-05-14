/* PR target/31507 */
/* { dg-do run } */
/* { dg-options "-Os -fno-omit-frame-pointer" } */

extern void abort (void);

__attribute__((noinline)) void
foo (double d0, double d1, double d2, double d3,
     double d4, double d5, double d6, double d7,
     float f0, float f1, float f2, float f3,
     char *p)
{
  if (d0 != 0 || d1 != 1 || d2 != 2 || d3 != 3)
    abort ();
  if (d4 != 4 || d5 != 5 || d6 != 6 || d7 != 7)
    abort ();
  if (f0 != 10 || f1 != 11 || f2 != 12 || f3 != 13)
    abort ();
  if (__builtin_memcmp (p, "foo", 4) != 0)
    abort ();
  __builtin_memcpy (p, "bar", 4);
}

__attribute__((noinline)) void
bar (int x)
{
  char p[x];
  if (x >= sizeof "foo")
    __builtin_memcpy (p, "foo", 4);
  foo (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0,
       10.0f, 11.0f, 12.0f, 13.0f, p);
  if (x >= sizeof "bar" && __builtin_memcmp (p, "bar", 4) != 0)
    abort ();
}

int
main (void)
{
  bar (128);
  return 0;
}
