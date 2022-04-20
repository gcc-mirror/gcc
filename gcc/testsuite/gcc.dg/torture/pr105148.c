/* { dg-do compile } */

extern void foo (void);

static inline int
bar (int n)
{
  for (int i = 0; i < n; i++)
    {
      foo ();
      int y[1][i];
      y[n][i] = 0;
    }
}

int
baz (void)
{
  return bar (5);
}
