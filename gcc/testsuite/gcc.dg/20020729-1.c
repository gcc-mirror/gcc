/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-O2 -march=k6" } */

static inline void *
baz (void *s, unsigned long c, unsigned int count)
{
  int d0, d1;
  __asm__ __volatile__ (""
			: "=&c" (d0), "=&D" (d1)
			:"a" (c), "q" (count), "0" (count / 4), "1" ((long) s)
			:"memory");
  return s;
}

struct A
{
  unsigned long *a;
};

inline static void *
bar (struct A *x, int y)
{
  char *ptr;

  ptr = (void *) x->a[y >> 12];
  ptr += y % (1UL << 12);
  return (void *) ptr;
}

int
foo (struct A *x, unsigned int *y, int z, int u)
{
  int a, b, c, d, e;

  z += *y;
  c = z + u;
  a = (z >> 12) + 1;
  do
    {
      b = (a << 12);
      d = b - z;
      e = c - z;
      if (e < d)
	d = e;
      baz (bar (x, z), 0, d);
      z = b;
      a++;
    }
  while (z < c);
  return 0;
}
