/* { dg-do compile } */
/* { dg-options "-O -ftree-parallelize-loops=4" } */

int d[1024];

static inline int foo (void)
{
  int s = 0;
  int i = 0;
  for (; i < 1024; i++)
    s += d[i];
  return s;
}

void bar (void)
{
  if (foo ())
    __builtin_abort ();
}
