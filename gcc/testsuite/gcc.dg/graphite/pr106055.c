/* { dg-do compile } */
/* { dg-options "-Os -floop-parallelize-all -fno-tree-dce" } */

__attribute__ ((returns_twice)) int
bar (void);

void
quux (void);

void
empty (void)
{
}

unsigned int
choose (unsigned int x, unsigned int y)
{
  return y ? x : 0;
}

int
foo (int *p, unsigned int x, int y)
{
  unsigned int acc = 0;

  empty ();

  while (x)
    {
      bar ();
      ++x;
    }

  while (y)
    acc += y;

  *p = choose (acc, 1);
  quux ();

  return x;
}
