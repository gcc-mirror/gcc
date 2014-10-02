/* { dg-options "-fgnu89-inline -Wno-pointer-to-int-cast" } */

extern void abort (void);
extern void exit (int);

int a[2];

__inline__ void f (int b, int o)
{
  unsigned int i;
  int *p;
  for (p = &a[b], i = b; --i < ~0; )
    *--p = i * 3 + o;
}

void
g(int b)
{
  f (b, (int)a);
}

int
main ()
{
  a[0] = a[1] = 0;
  g (2);
  if (a[0] != (int)a || a[1] != (int)a + 3)
    abort ();
  exit (0);
}
