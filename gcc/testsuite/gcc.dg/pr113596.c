/* PR middle-end/113596 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) void
bar (char *p, int n)
{
  p[0] = 1;
  p[n - 1] = 2;
}

static inline __attribute__((always_inline)) void
foo (int n)
{
  char *p = __builtin_alloca (n);
  bar (p, n);
}

int
main ()
{
  for (int i = 2; i < 8192; ++i)
    foo (i);
}
