/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-options "-O2 -fomit-frame-pointer -march=i386" { target i?86-*-* } } */

extern void abort (void);
extern void exit (int);

struct S
{
  int *a;
  unsigned char *b, c;
};

int u, v, w;

void
foo (unsigned short x)
{
  u += x;
}

int
bar (struct S **x, int *y)
{
  w += *y;
  *y = w + 25;
  return 0;
}

int
baz (struct S **x)
{
  struct S *y = *x;
  unsigned char *a = y->b;

  foo (*a);

  if (__builtin_expect (y->c != 0 || y->a == &v, 0))
    return 1;

  if (__builtin_expect (*a == 1, 0))
    {
      int a, b = bar (x, &a);

      if (a)
	return b;
    }

  return 0;
}

int
main (void)
{
  struct S a, *b = &a;
  unsigned char c;

  __builtin_memset (b, 0, sizeof (a));
  a.a = &v;
  a.b = &c;
  if (baz (&b) != 1)
    abort ();
  exit (0);
}
