/* PR rtl-optimization/48156 */
/* { dg-do run } */
/* { dg-options "-O -fcrossjumping --param min-crossjump-insns=1" } */

extern void abort (void);

static int __attribute__ ((noinline, noclone))
equals (int s1, int s2)
{
  return s1 == s2;
}

static int __attribute__ ((noinline, noclone))
bar (void)
{
  return 1;
}

static void __attribute__ ((noinline, noclone))
baz (int f, int j)
{
  if (f != 4 || j != 2)
    abort ();
}

void
foo (int x)
{
  int i = 0, j = bar ();

  if (x == 1)
    i = 2;

  if (j && equals (i, j))
    baz (8, i);
  else
    baz (4, i);
}

int
main ()
{
  foo (1);
  return 0;
}
