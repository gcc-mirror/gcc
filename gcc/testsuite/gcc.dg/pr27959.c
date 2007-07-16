/* PR middle-end/27959 */
/* { dg-do run { target { int32plus } } } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=z990" { target s390*-*-* } } */

extern void abort (void);

struct B
{
  unsigned int b1, b2, b3;
  char b4;
};

struct C
{
  char c1;
};

struct D
{
  char *d1;
  struct C **d2;
  unsigned int d3;
};

void
__attribute__((noinline))
foo (void *x, struct B *y, unsigned int *z)
{
  if (x)
    abort ();
  if (y->b1 != 7 || y->b2 != 5 || y->b3 != 3 || y->b4)
    abort ();
  if (*z != 2)
    abort ();
}

int
__attribute__((noinline))
baz (unsigned int *x, unsigned int y)
{
  asm volatile ("" : : "r" (&x), "r" (&y) : "memory");
  return *x + y;
}

inline int bar (unsigned int *x, unsigned int y)
{
  if (y < *x)
    return 0;
  return baz (x, y);
}

unsigned int *
__attribute__((noinline))
test (struct D *x, unsigned int *y)
{
  struct B b;
  unsigned int c;

  bar (y, x->d3);
  if ((*(x->d2))->c1)
    c = ((unsigned char) x->d1[0]
	 + ((unsigned char) x->d1[1] << 8)
	 + ((unsigned char) x->d1[2] << 16)
	 + ((short) x->d1[3] << 24));
  else
    {
      int d;
      ((char *) &d)[0] = x->d1[0];
      ((char *) &d)[1] = x->d1[1];
      ((char *) &d)[2] = x->d1[2];
      ((char *) &d)[3] = x->d1[3];
      c = d;
    }
  b.b4 = 0;
  b.b1 = c / 10000L % 10000;
  b.b2 = c / 100 % 100;
  b.b3 = c % 100;
  foo (0, &b, y);
  return y;
}

int
main (void)
{
  unsigned int x = 900070503;
  unsigned int y = 2;
  struct C c = { 0 }, *cptr = &c;
  struct D d = { (char *) &x, &cptr, 0 };
  if (test (&d, &y) != &y)
    abort ();
  return 0;
}
