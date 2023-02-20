/* { dg-do run } */
/* { dg-options "-O2 -Wno-stringop-overread" } */
/* { dg-require-effective-target alloca } */

#include "builtin-object-size-common.h"

struct A
{
  char a[10];
  int b;
  char c[10];
} y, w[4];

extern char exta[];
extern char extb[30];
extern struct A zerol[0];

void
__attribute__ ((noinline))
test1 (void *q, int x)
{
  struct A a;
  void *p = &a.a[3], *r;
  char var[x + 10];
  if (x < 0)
    r = &a.a[9];
  else
    r = &a.c[1];
  if (__builtin_object_size (p, 0)
      != sizeof (a) - __builtin_offsetof (struct A, a) - 3)
    FAIL ();
  if (__builtin_object_size (&a.c[9], 0)
      != sizeof (a) - __builtin_offsetof (struct A, c) - 9)
    FAIL ();
  if (__builtin_object_size (q, 0) != (size_t) -1)
    FAIL ();
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0)
      != (x < 0
	  ? sizeof (a) - __builtin_offsetof (struct A, a) - 9
	  : sizeof (a) - __builtin_offsetof (struct A, c) - 1))
    FAIL ();
#else
  if (__builtin_object_size (r, 0)
      != sizeof (a) - __builtin_offsetof (struct A, a) - 9)
    FAIL ();
#endif
  if (x < 6)
    r = &w[2].a[1];
  else
    r = &a.a[6];
  if (__builtin_object_size (&y, 0)
      != sizeof (y))
    FAIL ();
  if (__builtin_object_size (w, 0)
      != sizeof (w))
    FAIL ();
  if (__builtin_object_size (&y.b, 0)
      != sizeof (a) - __builtin_offsetof (struct A, b))
    FAIL ();
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0)
      != (x < 6
	  ? 2 * sizeof (w[0]) - __builtin_offsetof (struct A, a) - 1
	  : sizeof (a) - __builtin_offsetof (struct A, a) - 6))
    FAIL ();
#else
  if (__builtin_object_size (r, 0)
      != 2 * sizeof (w[0]) - __builtin_offsetof (struct A, a) - 1)
    FAIL ();
#endif
  if (x < 20)
    r = malloc (30);
  else
    r = calloc (2, 16);
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0) != (x < 20 ? 30 : 2 * 16))
    FAIL ();
#else
  /* We may duplicate this test onto the two exit paths.  On one path
     the size will be 32, the other it will be 30.  If we don't duplicate
     this test, then the size will be 32.  */
  if (__builtin_object_size (r, 0) != 2 * 16
      && __builtin_object_size (r, 0) != 30)
    FAIL ();
#endif
  if (x < 20)
    r = malloc (30);
  else
    r = calloc (2, 14);
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0) != (x < 20 ? 30 : 2 * 14))
    FAIL ();
#else
  if (__builtin_object_size (r, 0) != 30)
    FAIL ();
#endif
  if (x < 30)
    r = malloc (sizeof (a));
  else
    r = &a.a[3];
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0) != (x < 30 ? sizeof (a) : sizeof (a) - 3))
    FAIL ();
#else
  if (__builtin_object_size (r, 0) != sizeof (a))
    FAIL ();
#endif
  r = memcpy (r, "a", 2);
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0) != (x < 30 ? sizeof (a) : sizeof (a) - 3))
    FAIL ();
#else
  if (__builtin_object_size (r, 0) != sizeof (a))
    FAIL ();
#endif
  r = memcpy (r + 2, "b", 2) + 2;
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0)
      != (x < 30 ? sizeof (a) - 4 : sizeof (a) - 7))
    FAIL ();
#else
  if (__builtin_object_size (r, 0) != sizeof (a) - 4)
    FAIL ();
#endif
  r = &a.a[4];
  r = memset (r, 'a', 2);
  if (__builtin_object_size (r, 0)
      != sizeof (a) - __builtin_offsetof (struct A, a) - 4)
    FAIL ();
  r = memset (r + 2, 'b', 2) + 2;
  if (__builtin_object_size (r, 0)
      != sizeof (a) - __builtin_offsetof (struct A, a) - 8)
    FAIL ();
  r = &a.a[1];
  r = strcpy (r, "ab");
  if (__builtin_object_size (r, 0)
      != sizeof (a) - __builtin_offsetof (struct A, a) - 1)
    FAIL ();
  r = strcpy (r + 2, "cd") + 2;
  if (__builtin_object_size (r, 0)
      != sizeof (a) - __builtin_offsetof (struct A, a) - 5)
    FAIL ();
  if (__builtin_object_size (exta, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (exta + 10, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&exta[5], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (extb, 0) != sizeof (extb))
    FAIL ();
  if (__builtin_object_size (extb + 10, 0) != sizeof (extb) - 10)
    FAIL ();
  if (__builtin_object_size (&extb[5], 0) != sizeof (extb) - 5)
    FAIL ();
#ifdef __builtin_object_size
  if (__builtin_object_size (var, 0) != x + 10)
    FAIL ();
  if (__builtin_object_size (var + 10, 0) != x)
    FAIL ();
  if (__builtin_object_size (&var[5], 0) != x + 5)
    FAIL ();
#else
  if (__builtin_object_size (var, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (var + 10, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&var[5], 0) != (size_t) -1)
    FAIL ();
#endif
  if (__builtin_object_size (zerol, 0) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol, 0) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol[0], 0) != 0)
    FAIL ();
  if (__builtin_object_size (zerol[0].a, 0) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol[0].a[0], 0) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol[0].b, 0) != 0)
    FAIL ();
  if (__builtin_object_size ("abcdefg", 0) != sizeof ("abcdefg"))
    FAIL ();
  if (__builtin_object_size ("abcd\0efg", 0) != sizeof ("abcd\0efg"))
    FAIL ();
  if (__builtin_object_size (&"abcd\0efg", 0) != sizeof ("abcd\0efg"))
    FAIL ();
  if (__builtin_object_size (&"abcd\0efg"[0], 0) != sizeof ("abcd\0efg"))
    FAIL ();
  if (__builtin_object_size (&"abcd\0efg"[4], 0) != sizeof ("abcd\0efg") - 4)
    FAIL ();
  if (__builtin_object_size ("abcd\0efg" + 5, 0) != sizeof ("abcd\0efg") - 5)
    FAIL ();
  if (__builtin_object_size (L"abcdefg", 0) != sizeof (L"abcdefg"))
    FAIL ();
  r = (char *) L"abcd\0efg";
  if (__builtin_object_size (r + 2, 0) != sizeof (L"abcd\0efg") - 2)
    FAIL ();
}

size_t l1 = 1;

void
__attribute__ ((noinline))
test2 (void)
{
  struct B { char buf1[10]; char buf2[10]; } a;
  char *r, buf3[20];
  int i;
  size_t res;

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
#ifdef __builtin_object_size
  res = sizeof (buf3);

  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	res = sizeof (a) - __builtin_offsetof (struct B, buf1) - 1;
      else if (i == l1)
	res = sizeof (a) - __builtin_offsetof (struct B, buf2) - 7;
      else if (i == l1 + 1)
	res = sizeof (buf3) - 5;
      else if (i == l1 + 2)
	res = sizeof (a) - __builtin_offsetof (struct B, buf1) - 9;
    }
#else
  res = 20;
#endif
  if (__builtin_object_size (r, 0) != res)
    FAIL ();
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
#ifdef __builtin_object_size
  res = sizeof (buf3) - 20;

  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	res = sizeof (a) - __builtin_offsetof (struct B, buf1) - 7;
      else if (i == l1)
	res = sizeof (a) - __builtin_offsetof (struct B, buf2) - 7;
      else if (i == l1 + 1)
	res = sizeof (buf3) - 5;
      else if (i == l1 + 2)
	res = sizeof (a) - __builtin_offsetof (struct B, buf1) - 9;
    }
  if (__builtin_object_size (r, 0) != res)
    FAIL ();
#else
  res = 15;
#endif
  if (__builtin_object_size (r, 0) != res)
    FAIL ();
  r += 8;
#ifdef __builtin_object_size
  res -= 8;
  if (__builtin_object_size (r, 0) != res)
    FAIL ();
  if (res >= 6)
    {
      if (__builtin_object_size (r + 6, 0) != res - 6)
	FAIL ();
    }
  else if (__builtin_object_size (r + 6, 0) != 0)
    FAIL ();
#else
  if (__builtin_object_size (r, 0) != 7)
    FAIL ();
  if (__builtin_object_size (r + 6, 0) != 1)
    FAIL ();
#endif
  r = &buf3[18];
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[9];
      else if (i == l1)
	r = &a.buf2[9];
      else if (i == l1 + 1)
	r = &buf3[5];
      else if (i == l1 + 2)
	r = &a.buf1[4];
    }
#ifdef __builtin_object_size
  res = sizeof (buf3) - 18;

  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	  res = sizeof (a) - __builtin_offsetof (struct B, buf1) - 9;
      else if (i == l1)
	res = sizeof (a) - __builtin_offsetof (struct B, buf2) - 9;
      else if (i == l1 + 1)
	res = sizeof (buf3) - 5;
      else if (i == l1 + 2)
	res = sizeof (a) - __builtin_offsetof (struct B, buf1) - 4;
    }
  if (res >= 12)
    {
      if (__builtin_object_size (r + 12, 0) != res - 12)
	FAIL ();
    }
  else if (__builtin_object_size (r + 12, 0) != 0)
    FAIL ();
#else
  if (__builtin_object_size (r + 12, 0) != 4)
    FAIL ();
#endif
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

  if (__builtin_object_size (buf4, 0) != sizeof (buf4))
    FAIL ();
  if (__builtin_object_size (&buf4, 0) != sizeof (buf4))
    FAIL ();
  if (__builtin_object_size (&buf4[0], 0) != sizeof (buf4))
    FAIL ();
  if (__builtin_object_size (&buf4[1], 0) != sizeof (buf4) - 1)
    FAIL ();
  if (__builtin_object_size (&x, 0) != sizeof (x))
    FAIL ();
  if (__builtin_object_size (&x.a, 0) != sizeof (x))
    FAIL ();
  if (__builtin_object_size (&x.a[0], 0) != sizeof (x))
    FAIL ();
  if (__builtin_object_size (&x.a[0].a, 0) != sizeof (x))
    FAIL ();
  if (__builtin_object_size (&x.a[0].a[0], 0) != sizeof (x))
    FAIL ();
  if (__builtin_object_size (&x.a[0].a[3], 0) != sizeof (x) - 3)
    FAIL ();
  if (__builtin_object_size (&x.a[0].b, 0)
      != sizeof (x) - __builtin_offsetof (struct A, b))
    FAIL ();
  if (__builtin_object_size (&x.a[1].c, 0)
      != sizeof (x) - sizeof (struct A) - __builtin_offsetof (struct A, c))
    FAIL ();
  if (__builtin_object_size (&x.a[1].c[0], 0)
      != sizeof (x) - sizeof (struct A) - __builtin_offsetof (struct A, c))
    FAIL ();
  if (__builtin_object_size (&x.a[1].c[3], 0)
      != sizeof (x) - sizeof (struct A) - __builtin_offsetof (struct A, c) - 3)
    FAIL ();
  if (__builtin_object_size (&x.b, 0)
      != sizeof (x) - __builtin_offsetof (struct B, b))
    FAIL ();
  if (__builtin_object_size (&x.b.a, 0)
      != sizeof (x) - __builtin_offsetof (struct B, b))
    FAIL ();
  if (__builtin_object_size (&x.b.a[0], 0)
      != sizeof (x) - __builtin_offsetof (struct B, b))
    FAIL ();
  if (__builtin_object_size (&x.b.a[3], 0)
      != sizeof (x) - __builtin_offsetof (struct B, b) - 3)
    FAIL ();
  if (__builtin_object_size (&x.b.b, 0)
      != sizeof (x) - __builtin_offsetof (struct B, b)
	 - __builtin_offsetof (struct A, b))
    FAIL ();
  if (__builtin_object_size (&x.b.c, 0)
      != sizeof (x) - __builtin_offsetof (struct B, b)
	 - __builtin_offsetof (struct A, c))
    FAIL ();
  if (__builtin_object_size (&x.b.c[0], 0)
      != sizeof (x) - __builtin_offsetof (struct B, b)
	 - __builtin_offsetof (struct A, c))
    FAIL ();
  if (__builtin_object_size (&x.b.c[3], 0)
      != sizeof (x) - __builtin_offsetof (struct B, b)
	 - __builtin_offsetof (struct A, c) - 3)
    FAIL ();
  if (__builtin_object_size (&x.c, 0)
      != sizeof (x) - __builtin_offsetof (struct B, c))
    FAIL ();
  if (__builtin_object_size (&x.c[0], 0)
      != sizeof (x) - __builtin_offsetof (struct B, c))
    FAIL ();
  if (__builtin_object_size (&x.c[1], 0)
      != sizeof (x) - __builtin_offsetof (struct B, c) - 1)
    FAIL ();
  if (__builtin_object_size (&x.d, 0)
      != sizeof (x) - __builtin_offsetof (struct B, d))
    FAIL ();
  if (__builtin_object_size (&x.e, 0)
      != sizeof (x) - __builtin_offsetof (struct B, e))
    FAIL ();
  if (__builtin_object_size (&x.f, 0)
      != sizeof (x) - __builtin_offsetof (struct B, f))
    FAIL ();
  dp = &__real__ x.f;
  if (__builtin_object_size (dp, 0)
      != sizeof (x) - __builtin_offsetof (struct B, f))
    FAIL ();
  dp = &__imag__ x.f;
  if (__builtin_object_size (dp, 0)
      != sizeof (x) - __builtin_offsetof (struct B, f)
	 - sizeof (x.f) / 2)
    FAIL ();
  dp = &y;
  if (__builtin_object_size (dp, 0) != sizeof (y))
    FAIL ();
  if (__builtin_object_size (&z, 0) != sizeof (z))
    FAIL ();
  dp = &__real__ z;
  if (__builtin_object_size (dp, 0) != sizeof (z))
    FAIL ();
  dp = &__imag__ z;
  if (__builtin_object_size (dp, 0) != sizeof (z) / 2)
    FAIL ();
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
      if (__builtin_object_size (p, 0) != (size_t) -1)
	FAIL ();
    }
  return x;
}

void
__attribute__ ((noinline))
test5 (size_t x)
{
  char buf[64];
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 0) != sizeof (buf) - 8 - 4 * x)
    FAIL ();
#else
  /* My understanding of ISO C99 6.5.6 is that a conforming
     program will not end up with p equal to &buf[0]
     through &buf[7], i.e. calling this function with say
     UINTPTR_MAX / 4 results in undefined behavior.
     If that's true, then the maximum number of remaining
     bytes from p until end of the object is 56, otherwise
     it would be 64 (or conservative (size_t) -1 == unknown).  */
  if (__builtin_object_size (p, 0) != sizeof (buf) - 8)
    FAIL ();
#endif
  memset (p, ' ', sizeof (buf) - 8 - 4 * 4);
}

void
__attribute__ ((noinline))
test6 (size_t x)
{
  struct T { char buf[64]; char buf2[64]; } t;
  char *p = &t.buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 0) != sizeof (t) - 8 - 4 * x)
    FAIL ();
#else
  if (__builtin_object_size (p, 0) != sizeof (t) - 8)
    FAIL ();
#endif
  memset (p, ' ', sizeof (t) - 8 - 4 * 4);
}

void
__attribute__ ((noinline))
test7 (void)
{
  char buf[64];
  struct T { char buf[64]; char buf2[64]; } t;
  char *p = &buf[64], *q = &t.buf[64];

  if (__builtin_object_size (p + 64, 0) != 0)
    FAIL ();
  if (__builtin_object_size (q + 63, 0) != sizeof (t) - 64 - 63)
    FAIL ();
  if (__builtin_object_size (q + 64, 0) != sizeof (t) - 64 - 64)
    FAIL ();
  if (__builtin_object_size (q + 256, 0) != 0)
    FAIL ();
}

void
__attribute__ ((noinline))
test8 (void)
{
  struct T { char buf[10]; char buf2[10]; } t;
  char *p = &t.buf2[-4];
  char *q = &t.buf2[0];
  if (__builtin_object_size (p, 0) != sizeof (t) - 10 + 4)
    FAIL ();
  if (__builtin_object_size (q, 0) != sizeof (t) - 10)
    FAIL ();
  /* GCC only handles additions, not subtractions.  */
  q = q - 8;
  if (__builtin_object_size (q, 0) != (size_t) -1
      && __builtin_object_size (q, 0) != sizeof (t) - 10 + 8)
    FAIL ();
  p = &t.buf[-4];
  if (__builtin_object_size (p, 0) != 0)
    FAIL ();
}

void
__attribute__ ((noinline))
test9 (unsigned cond)
{
  char *buf2 = malloc (10);
  char *p;

  if (cond)
    p = &buf2[8];
  else
    p = &buf2[4];

#ifdef __builtin_object_size
  if (__builtin_object_size (&p[-4], 0) != (cond ? 6 : 10))
    FAIL ();
#else
  if (__builtin_object_size (&p[-4], 0) != 10)
    FAIL ();
#endif

  for (unsigned i = cond; i > 0; i--)
    p--;

#ifdef __builtin_object_size
  if (__builtin_object_size (p, 0) != ((cond ? 2 : 6) + cond))
    FAIL ();
#else
  if (__builtin_object_size (p, 0) != 10)
    FAIL ();
#endif

  p = &y.c[8];
  for (unsigned i = cond; i > 0; i--)
    p--;

#ifdef __builtin_object_size
  if (__builtin_object_size (p, 0)
      != sizeof (y) - __builtin_offsetof (struct A, c) - 8 + cond)
    FAIL ();
#else
  if (__builtin_object_size (p, 0) != sizeof (y))
    FAIL ();
#endif
}

void
__attribute__ ((noinline))
test10 (void)
{
  static char buf[255];
  unsigned int i, len = sizeof (buf);
  char *p = buf;

  for (i = 0 ; i < sizeof (buf) ; i++)
    {
      if (len < 2)
	{
#ifdef __builtin_object_size
	  if (__builtin_object_size (p - 3, 0) != sizeof (buf) - i + 3)
	    FAIL ();
#else
	  if (__builtin_object_size (p - 3, 0) != sizeof (buf))
	    FAIL ();
#endif
	  break;
	}
      p++;
      len--;
    }
}

/* Tests for strdup/strndup.  */
size_t
__attribute__ ((noinline))
test11 (void)
{
  int i = 0;
  const char *ptr = "abcdefghijklmnopqrstuvwxyz";
  char *res = strndup (ptr, 21);
  if (__builtin_object_size (res, 0) != 22)
    FAIL ();

  free (res);

  res = strndup (ptr, 32);
  if (__builtin_object_size (res, 0) != 27)
    FAIL ();

  free (res);

  res = strdup (ptr);
  if (__builtin_object_size (res, 0) != 27)
    FAIL ();

  free (res);

  char *ptr2 = malloc (64);
  strcpy (ptr2, ptr);

  res = strndup (ptr2, 21);
  if (__builtin_object_size (res, 0) != 22)
    FAIL ();

  free (res);

  res = strndup (ptr2, 32);
  if (__builtin_object_size (res, 0) != 33)
    FAIL ();

  free (res);

  res = strndup (ptr2, 128);
  if (__builtin_object_size (res, 0) != 64)
    FAIL ();

  free (res);

  res = strdup (ptr2);
#ifdef __builtin_object_size
  if (__builtin_object_size (res, 0) != 27)
#else
  if (__builtin_object_size (res, 0) != (size_t) -1)
#endif
    FAIL ();
  free (res);
  free (ptr2);

  ptr = "abcd\0efghijklmnopqrstuvwxyz";
  res = strdup (ptr);
  if (__builtin_object_size (res, 0) != 5)
    FAIL ();
  free (res);

  res = strndup (ptr, 24);
  if (__builtin_object_size (res, 0) != 5)
    FAIL ();
  free (res);

  res = strndup (ptr, 2);
  if (__builtin_object_size (res, 0) != 3)
    FAIL ();
  free (res);

  res = strdup (&ptr[4]);
  if (__builtin_object_size (res, 0) != 1)
    FAIL ();
  free (res);

  res = strndup (&ptr[4], 4);
  if (__builtin_object_size (res, 0) != 1)
    FAIL ();
  free (res);

  res = strndup (&ptr[4], 1);
  if (__builtin_object_size (res, 0) != 1)
    FAIL ();
  free (res);
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
  test6 (4);
  test7 ();
  test8 ();
  test9 (1);
  test10 ();
  test11 ();
  DONE ();
}
