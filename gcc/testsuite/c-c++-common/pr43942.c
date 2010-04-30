/* PR debug/43942 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

extern int f1 (int);

int
f2 (int x)
{
  extern int v;
  return f1 (x);
}

void
f3 (void)
{
  f2 (0);
}

static inline int
f4 (int x)
{
  extern int w;
  if (w)
    return f1 (x);
  return 0;
}

void
f5 (void)
{
  f4 (0);
}
