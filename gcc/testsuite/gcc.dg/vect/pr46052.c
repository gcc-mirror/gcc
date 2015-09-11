/* { dg-do compile } */

int i;
int a[2];

static inline char bar (void)
{
  return i ? i : 1;
}

void foo (int n)
{
  while (n--)
    {
      a[0] ^= bar ();
      a[1] ^= bar ();
    }
}

static inline char bar1 (void)
{
}

void foo1 (int n)
{
  while (n--)
    {
      a[0] ^= bar1 ();
      a[1] ^= bar1 ();
    }
}

