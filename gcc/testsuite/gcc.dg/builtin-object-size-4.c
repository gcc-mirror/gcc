/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target alloca } */

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern void exit (int);
extern void *malloc (size_t);
extern void *calloc (size_t, size_t);
extern void *alloca (size_t);
extern void *memcpy (void *, const void *, size_t);
extern void *memset (void *, int, size_t);
extern char *strcpy (char *, const char *);

struct A
{
  char a[10];
  int b;
  char c[10];
} y, w[4];

extern char exta[];
extern char extb[30];
extern struct A extc[];
struct A zerol[0];

void
__attribute__ ((noinline))
test1 (void *q, int x)
{
  struct A a;
  void *p = &a.a[3], *r;
  char var[x + 10];
  struct A vara[x + 10];
  if (x < 0)
    r = &a.a[9];
  else
    r = &a.c[1];
  if (__builtin_object_size (p, 3) != sizeof (a.a) - 3)
    abort ();
  if (__builtin_object_size (&a.c[9], 3)
      != sizeof (a.c) - 9)
    abort ();
  if (__builtin_object_size (q, 3) != 0)
    abort ();
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 9)
    abort ();
  if (x < 6)
    r = &w[2].a[1];
  else
    r = &a.a[6];
  if (__builtin_object_size (&y, 3) != sizeof (y))
    abort ();
  if (__builtin_object_size (w, 3) != sizeof (w))
    abort ();
  if (__builtin_object_size (&y.b, 3) != sizeof (a.b))
    abort ();
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 6)
    abort ();
  if (x < 20)
    r = malloc (30);
  else
    r = calloc (2, 16);
  if (__builtin_object_size (r, 3) != 30)
    abort ();
  if (x < 20)
    r = malloc (30);
  else
    r = calloc (2, 14);
  if (__builtin_object_size (r, 3) != 2 * 14)
    abort ();
  if (x < 30)
    r = malloc (sizeof (a));
  else
    r = &a.a[3];
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 3)
    abort ();
  r = memcpy (r, "a", 2);
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 3)
    abort ();
  r = memcpy (r + 2, "b", 2) + 2;
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 3 - 4)
    abort ();
  r = &a.a[4];
  r = memset (r, 'a', 2);
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 4)
    abort ();
  r = memset (r + 2, 'b', 2) + 2;
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 8)
    abort ();
  r = &a.a[1];
  r = strcpy (r, "ab");
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 1)
    abort ();
  r = strcpy (r + 2, "cd") + 2;
  if (__builtin_object_size (r, 3) != sizeof (a.a) - 5)
    abort ();
  if (__builtin_object_size (exta, 3) != 0)
    abort ();
  if (__builtin_object_size (exta + 10, 3) != 0)
    abort ();
  if (__builtin_object_size (&exta[5], 3) != 0)
    abort ();
  if (__builtin_object_size (extb, 3) != sizeof (extb))
    abort ();
  if (__builtin_object_size (extb + 10, 3) != sizeof (extb) - 10)
    abort ();
  if (__builtin_object_size (&extb[5], 3) != sizeof (extb) - 5)
    abort ();
  if (__builtin_object_size (extc, 3) != 0)
    abort ();
  if (__builtin_object_size (extc + 10, 3) != 0)
    abort ();
  if (__builtin_object_size (&extc[5], 3) != 0)
    abort ();
  if (__builtin_object_size (&extc->a, 3) != 0)
    abort ();
  if (__builtin_object_size (&(extc + 10)->b, 3) != 0)
    abort ();
  if (__builtin_object_size (&extc[5].c[3], 3) != 0)
    abort ();
  if (__builtin_object_size (var, 3) != 0)
    abort ();
  if (__builtin_object_size (var + 10, 3) != 0)
    abort ();
  if (__builtin_object_size (&var[5], 3) != 0)
    abort ();
  if (__builtin_object_size (vara, 3) != 0)
    abort ();
  if (__builtin_object_size (vara + 10, 3) != 0)
    abort ();    
  if (__builtin_object_size (&vara[5], 3) != 0)
    abort ();
  if (__builtin_object_size (&vara[0].a, 3) != sizeof (vara[0].a))
    abort ();
  if (__builtin_object_size (&vara[10].a[0], 3) != sizeof (vara[0].a))
    abort ();
  if (__builtin_object_size (&vara[5].a[4], 3) != sizeof (vara[0].a) - 4)
    abort ();
  if (__builtin_object_size (&vara[5].b, 3) != sizeof (vara[0].b))
    abort ();
  if (__builtin_object_size (&vara[7].c[7], 3) != sizeof (vara[0].c) - 7)
    abort ();
  if (__builtin_object_size (zerol, 3) != 0)
    abort ();
  if (__builtin_object_size (&zerol, 3) != 0)
    abort ();
  if (__builtin_object_size (&zerol[0], 3) != 0)
    abort ();
  if (__builtin_object_size (zerol[0].a, 3) != 0)
    abort ();
  if (__builtin_object_size (&zerol[0].a[0], 3) != 0)
    abort ();
  if (__builtin_object_size (&zerol[0].b, 3) != 0)
    abort ();
  if (__builtin_object_size ("abcdefg", 3) != sizeof ("abcdefg"))
    abort ();
  if (__builtin_object_size ("abcd\0efg", 3) != sizeof ("abcd\0efg"))
    abort ();
  if (__builtin_object_size (&"abcd\0efg", 3) != sizeof ("abcd\0efg"))
    abort ();
  if (__builtin_object_size (&"abcd\0efg"[0], 3) != sizeof ("abcd\0efg"))
    abort ();
  if (__builtin_object_size (&"abcd\0efg"[4], 3) != sizeof ("abcd\0efg") - 4)
    abort ();
  if (__builtin_object_size ("abcd\0efg" + 5, 3) != sizeof ("abcd\0efg") - 5)
    abort ();
  if (__builtin_object_size (L"abcdefg", 3) != sizeof (L"abcdefg"))
    abort ();
  r = (char *) L"abcd\0efg";
  if (__builtin_object_size (r + 2, 3) != sizeof (L"abcd\0efg") - 2)
    abort ();
  /* Prevent DSE from removing calls that prevent bad combining of
     addresses and offsets.  */
  asm volatile ("" : : "g" (&a));
}

size_t l1 = 1;

void
__attribute__ ((noinline))
test2 (void)
{
  struct B { char buf1[10]; char buf2[10]; } a;
  char *r, buf3[20];
  int i;

  if (sizeof (a) != 20)
    return;

  r = buf3;
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[1];
      else if (i == l1)
	r = &a.buf2[7];
      else if (i == l1 + 1)
	r = &buf3[5];
      else if (i == l1 + 2)
	r = &a.buf1[9];
    }
  if (__builtin_object_size (r, 3) != sizeof (a.buf1) - 9)
    abort ();
  r = &buf3[20];
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[7];
      else if (i == l1)
	r = &a.buf2[7];
      else if (i == l1 + 1)
	r = &buf3[5];
      else if (i == l1 + 2)
	r = &a.buf1[9];
    }
  if (__builtin_object_size (r, 3) != 0)
    abort ();
  r = &buf3[1];
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[6];
      else if (i == l1)
	r = &a.buf2[4];
      else if (i == l1 + 1)
	r = &buf3[5];
      else if (i == l1 + 2)
	r = &a.buf1[2];
    }
  if (__builtin_object_size (r, 3) != sizeof (a.buf1) - 6)
    abort ();
  r += 2;
  if (__builtin_object_size (r, 3) != sizeof (a.buf1) - 6 - 2)
    abort ();
  if (__builtin_object_size (r + 1, 3) != sizeof (a.buf1) - 6 - 3)
    abort ();
}

void
__attribute__ ((noinline))
test3 (void)
{
  char buf4[10];
  struct B { struct A a[2]; struct A b; char c[4]; char d; double e;
	     _Complex double f; } x;
  double y;
  _Complex double z;
  double *dp;

  if (__builtin_object_size (buf4, 3) != sizeof (buf4))
    abort ();
  if (__builtin_object_size (&buf4, 3) != sizeof (buf4))
    abort ();
  if (__builtin_object_size (&buf4[0], 3) != sizeof (buf4))
    abort ();
  if (__builtin_object_size (&buf4[1], 3) != sizeof (buf4) - 1)
    abort ();
  if (__builtin_object_size (&x, 3) != sizeof (x))
    abort ();
  if (__builtin_object_size (&x.a, 3) != sizeof (x.a))
    abort ();
  if (__builtin_object_size (&x.a[0], 3) != sizeof (x.a))
    abort ();
  if (__builtin_object_size (&x.a[0].a, 3) != sizeof (x.a[0].a))
    abort ();
  if (__builtin_object_size (&x.a[0].a[0], 3) != sizeof (x.a[0].a))
    abort ();
  if (__builtin_object_size (&x.a[0].a[3], 3) != sizeof (x.a[0].a) - 3)
    abort ();
  if (__builtin_object_size (&x.a[0].b, 3) != sizeof (x.a[0].b))
    abort ();
  if (__builtin_object_size (&x.a[1].c, 3) != sizeof (x.a[1].c))
    abort ();
  if (__builtin_object_size (&x.a[1].c[0], 3) != sizeof (x.a[1].c))
    abort ();
  if (__builtin_object_size (&x.a[1].c[3], 3) != sizeof (x.a[1].c) - 3)
    abort ();
  if (__builtin_object_size (&x.b, 3) != sizeof (x.b))
    abort ();
  if (__builtin_object_size (&x.b.a, 3) != sizeof (x.b.a))
    abort ();
  if (__builtin_object_size (&x.b.a[0], 3) != sizeof (x.b.a))
    abort ();
  if (__builtin_object_size (&x.b.a[3], 3) != sizeof (x.b.a) - 3)
    abort ();
  if (__builtin_object_size (&x.b.b, 3) != sizeof (x.b.b))
    abort ();
  if (__builtin_object_size (&x.b.c, 3) != sizeof (x.b.c))
    abort ();
  if (__builtin_object_size (&x.b.c[0], 3) != sizeof (x.b.c))
    abort ();
  if (__builtin_object_size (&x.b.c[3], 3) != sizeof (x.b.c) - 3)
    abort ();
  if (__builtin_object_size (&x.c, 3) != sizeof (x.c))
    abort ();
  if (__builtin_object_size (&x.c[0], 3) != sizeof (x.c))
    abort ();
  if (__builtin_object_size (&x.c[1], 3) != sizeof (x.c) - 1)
    abort ();
  if (__builtin_object_size (&x.d, 3) != sizeof (x.d))
    abort ();
  if (__builtin_object_size (&x.e, 3) != sizeof (x.e))
    abort ();
  if (__builtin_object_size (&x.f, 3) != sizeof (x.f))
    abort ();
  dp = &__real__ x.f;
  if (__builtin_object_size (dp, 3) != sizeof (x.f) / 2)
    abort ();
  dp = &__imag__ x.f;
  if (__builtin_object_size (dp, 3) != sizeof (x.f) / 2)
    abort ();
  dp = &y;
  if (__builtin_object_size (dp, 3) != sizeof (y))
    abort ();
  if (__builtin_object_size (&z, 3) != sizeof (z))
      abort ();
  dp = &__real__ z;
  if (__builtin_object_size (dp, 3) != sizeof (z) / 2)
    abort ();
  dp = &__imag__ z;
  if (__builtin_object_size (dp, 3) != sizeof (z) / 2)
    abort ();
}

struct S { unsigned int a; };

char *
__attribute__ ((noinline))
test4 (char *x, int y)
{
  register int i;
  struct A *p;

  for (i = 0; i < y; i++)
    {
      p = (struct A *) x;
      x = (char *) &p[1];
      if (__builtin_object_size (p, 3) != 0)
	abort ();
    }
  return x;
}

void
__attribute__ ((noinline))
test5 (size_t x)
{
  struct T { char buf[64]; char buf2[64]; } t;
  char *p = &t.buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
  if (__builtin_object_size (p, 3) != 0)
    abort ();
  memset (p, ' ', sizeof (t.buf) - 8 - 4 * 4);
}

void
__attribute__ ((noinline))
test6 (void)
{
  char buf[64];
  struct T { char buf[64]; char buf2[64]; } t;
  char *p = &buf[64], *q = &t.buf[64];

  if (__builtin_object_size (p + 64, 3) != 0)
    abort ();
  if (__builtin_object_size (q + 0, 3) != 0)
    abort ();
  if (__builtin_object_size (q + 64, 3) != 0)
    abort ();
}

void
__attribute__ ((noinline))
test7 (void)
{
  struct T { char buf[10]; char buf2[10]; } t;
  char *p = &t.buf2[-4];
  char *q = &t.buf2[0];
  if (__builtin_object_size (p, 3) != 0)
    abort ();
  if (__builtin_object_size (q, 3) != sizeof (t.buf2))
    abort ();
  q = &t.buf[10];
  if (__builtin_object_size (q, 3) != 0)
    abort ();
  q = &t.buf[11];
  if (__builtin_object_size (q, 3) != 0)
    abort ();
  p = &t.buf[-4];
  if (__builtin_object_size (p, 3) != 0)
    abort ();
}

int
main (void)
{
  struct S s[10];
  __asm ("" : "=r" (l1) : "0" (l1));
  test1 (main, 6);
  test2 ();
  test3 ();
  test4 ((char *) s, 10);
  test5 (4);
  test6 ();
  test7 ();
  exit (0);
}
