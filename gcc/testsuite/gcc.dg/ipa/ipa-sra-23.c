/* { dg-do compile } */
/* { dg-options "-O2"  } */

extern int g;

static int __attribute__((noinline))
bar (int i, int j)
{
  return 2*g + i;
}

static int __attribute__((noinline))
foo (int i, int j)
{
  if (i > 5)
    j = 22;
  return bar (i, j) + 1;
}

int
entry (int l, int k)
{
  return foo (l, k);
}
