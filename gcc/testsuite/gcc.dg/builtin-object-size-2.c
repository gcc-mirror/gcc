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
  if (__builtin_object_size (p, 1) != sizeof (a.a) - 3)
    FAIL ();
  if (__builtin_object_size (&a.c[9], 1)
      != sizeof (a.c) - 9)
    FAIL ();
  if (__builtin_object_size (q, 1) != (size_t) -1)
    FAIL ();
#ifdef __builtin_object_size
  if (x < 0
      ? __builtin_object_size (r, 1) != sizeof (a.a) - 9
      : __builtin_object_size (r, 1) != sizeof (a.c) - 1)
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (a.c) - 1)
    FAIL ();
#endif
  if (x < 6)
    r = &w[2].a[1];
  else
    r = &a.a[6];
  if (__builtin_object_size (&y, 1) != sizeof (y))
    FAIL ();
  if (__builtin_object_size (w, 1) != sizeof (w))
    FAIL ();
  if (__builtin_object_size (&y.b, 1) != sizeof (a.b))
    FAIL ();
#ifdef __builtin_object_size
  if (x < 6
      ? __builtin_object_size (r, 1) != sizeof (a.a) - 1
      : __builtin_object_size (r, 1) != sizeof (a.a) - 6)
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (a.a) - 1)
    FAIL ();
#endif
  if (x < 20)
    r = malloc (30);
  else
    r = calloc (2, 16);
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 1) != (x < 20 ? 30 : 2 * 16))
    FAIL ();
#else
  /* We may duplicate this test onto the two exit paths.  On one path
     the size will be 32, the other it will be 30.  If we don't duplicate
     this test, then the size will be 32.  */
  if (__builtin_object_size (r, 1) != 2 * 16
      && __builtin_object_size (r, 1) != 30)
    FAIL ();
#endif
  if (x < 20)
    r = malloc (30);
  else
    r = calloc (2, 14);
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 1) != (x < 20 ? 30 : 2 * 14))
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != 30)
    FAIL ();
#endif
  if (x < 30)
    r = malloc (sizeof (a));
  else
    r = &a.a[3];
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 1) != (x < 30 ? sizeof (a) : sizeof (a) - 3))
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (a))
    FAIL ();
#endif
  r = memcpy (r, "a", 2);
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 1) != (x < 30 ? sizeof (a) : sizeof (a) - 3))
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (a))
    FAIL ();
#endif
  r = memcpy (r + 2, "b", 2) + 2;
#ifdef __builtin_object_size
  if (__builtin_object_size (r, 0)
      != (x < 30 ? sizeof (a) - 4 : sizeof (a) - 7))
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (a) - 4)
    FAIL ();
#endif
  r = &a.a[4];
  r = memset (r, 'a', 2);
  if (__builtin_object_size (r, 1) != sizeof (a.a) - 4)
    FAIL ();
  r = memset (r + 2, 'b', 2) + 2;
  if (__builtin_object_size (r, 1) != sizeof (a.a) - 8)
    FAIL ();
  r = &a.a[1];
  r = strcpy (r, "ab");
  if (__builtin_object_size (r, 1) != sizeof (a.a) - 1)
    FAIL ();
  r = strcpy (r + 2, "cd") + 2;
  if (__builtin_object_size (r, 1) != sizeof (a.a) - 5)
    FAIL ();
  if (__builtin_object_size (exta, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (exta + 10, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&exta[5], 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (extb, 1) != sizeof (extb))
    FAIL ();
  if (__builtin_object_size (extb + 10, 1) != sizeof (extb) - 10)
    FAIL ();
  if (__builtin_object_size (&extb[5], 1) != sizeof (extb) - 5)
    FAIL ();
  if (__builtin_object_size (extc, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (extc + 10, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&extc[5], 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&extc->a, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&(extc + 10)->b, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&extc[5].c[3], 1) != (size_t) -1)
    FAIL ();
#ifdef __builtin_object_size
  if (__builtin_object_size (var, 1) != x + 10)
    FAIL ();
  if (__builtin_object_size (var + 10, 1) != x)
    FAIL ();
  if (__builtin_object_size (&var[5], 1) != x + 5)
    FAIL ();
  if (__builtin_object_size (vara, 1) != (x + 10) * sizeof (struct A))
    FAIL ();
  if (__builtin_object_size (vara + 10, 1) != x * sizeof (struct A))
    FAIL ();    
  if (__builtin_object_size (&vara[5], 1) != (x + 5) * sizeof (struct A))
    FAIL ();
#else
  if (__builtin_object_size (var, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (var + 10, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&var[5], 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (vara, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (vara + 10, 1) != (size_t) -1)
    FAIL ();    
  if (__builtin_object_size (&vara[5], 1) != (size_t) -1)
    FAIL ();
#endif
  if (__builtin_object_size (&vara[0].a, 1) != sizeof (vara[0].a))
    FAIL ();
  if (__builtin_object_size (&vara[10].a[0], 1) != sizeof (vara[0].a))
    FAIL ();
  if (__builtin_object_size (&vara[5].a[4], 1) != sizeof (vara[0].a) - 4)
    FAIL ();
  if (__builtin_object_size (&vara[5].b, 1) != sizeof (vara[0].b))
    FAIL ();
  if (__builtin_object_size (&vara[7].c[7], 1) != sizeof (vara[0].c) - 7)
    FAIL ();
  if (__builtin_object_size (zerol, 1) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol, 1) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol[0], 1) != 0)
    FAIL ();
  if (__builtin_object_size (zerol[0].a, 1) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol[0].a[0], 1) != 0)
    FAIL ();
  if (__builtin_object_size (&zerol[0].b, 1) != 0)
    FAIL ();
  if (__builtin_object_size ("abcdefg", 1) != sizeof ("abcdefg"))
    FAIL ();
  if (__builtin_object_size ("abcd\0efg", 1) != sizeof ("abcd\0efg"))
    FAIL ();
  if (__builtin_object_size (&"abcd\0efg", 1) != sizeof ("abcd\0efg"))
    FAIL ();
  if (__builtin_object_size (&"abcd\0efg"[0], 1) != sizeof ("abcd\0efg"))
    FAIL ();
  if (__builtin_object_size (&"abcd\0efg"[4], 1) != sizeof ("abcd\0efg") - 4)
    FAIL ();
  if (__builtin_object_size ("abcd\0efg" + 5, 1) != sizeof ("abcd\0efg") - 5)
    FAIL ();
  if (__builtin_object_size (L"abcdefg", 1) != sizeof (L"abcdefg"))
    FAIL ();
  r = (char *) L"abcd\0efg";
  if (__builtin_object_size (r + 2, 1) != sizeof (L"abcd\0efg") - 2)
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
#ifdef __builtin_object_size
  size_t dyn_res;
#endif

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
  dyn_res = sizeof (buf3);

  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	dyn_res = sizeof (a.buf1) - 1;
      else if (i == l1)
	dyn_res = sizeof (a.buf2) - 7;
      else if (i == l1 + 1)
	dyn_res = sizeof (buf3) - 5;
      else if (i == l1 + 2)
	dyn_res = sizeof (a.buf1) - 9;
    }
  if (__builtin_object_size (r, 1) != dyn_res)
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (buf3))
    FAIL ();
#endif
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
  dyn_res = sizeof (buf3) - 20;

  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	dyn_res = sizeof (a.buf1) - 7;
      else if (i == l1)
	dyn_res = sizeof (a.buf2) - 7;
      else if (i == l1 + 1)
	dyn_res = sizeof (buf3) - 5;
      else if (i == l1 + 2)
	dyn_res = sizeof (a.buf1) - 9;
    }
  if (__builtin_object_size (r, 1) != dyn_res)
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (buf3) - 5)
    FAIL ();
#endif
  r += 8;
#ifdef __builtin_object_size
  if (dyn_res >= 8)
    {
      dyn_res -= 8;
      if (__builtin_object_size (r, 1) != dyn_res)
	FAIL ();

      if (dyn_res >= 6)
	{
	  if (__builtin_object_size (r + 6, 1) != dyn_res - 6)
	    FAIL ();
	}
      else if (__builtin_object_size (r + 6, 1) != 0)
	FAIL ();
    }
  else if (__builtin_object_size (r, 1) != 0)
    FAIL ();
#else
  if (__builtin_object_size (r, 1) != sizeof (buf3) - 13)
    FAIL ();
  if (__builtin_object_size (r + 6, 1) != sizeof (buf3) - 19)
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

  if (__builtin_object_size (buf4, 1) != sizeof (buf4))
    FAIL ();
  if (__builtin_object_size (&buf4, 1) != sizeof (buf4))
    FAIL ();
  if (__builtin_object_size (&buf4[0], 1) != sizeof (buf4))
    FAIL ();
  if (__builtin_object_size (&buf4[1], 1) != sizeof (buf4) - 1)
    FAIL ();
  if (__builtin_object_size (&x, 1) != sizeof (x))
    FAIL ();
  if (__builtin_object_size (&x.a, 1) != sizeof (x.a))
    FAIL ();
  if (__builtin_object_size (&x.a[0], 1) != sizeof (x.a))
    FAIL ();
  if (__builtin_object_size (&x.a[0].a, 1) != sizeof (x.a[0].a))
    FAIL ();
  if (__builtin_object_size (&x.a[0].a[0], 1) != sizeof (x.a[0].a))
    FAIL ();
  if (__builtin_object_size (&x.a[0].a[3], 1) != sizeof (x.a[0].a) - 3)
    FAIL ();
  if (__builtin_object_size (&x.a[0].b, 1) != sizeof (x.a[0].b))
    FAIL ();
  if (__builtin_object_size (&x.a[1].c, 1) != sizeof (x.a[1].c))
    FAIL ();
  if (__builtin_object_size (&x.a[1].c[0], 1) != sizeof (x.a[1].c))
    FAIL ();
  if (__builtin_object_size (&x.a[1].c[3], 1) != sizeof (x.a[1].c) - 3)
    FAIL ();
  if (__builtin_object_size (&x.b, 1) != sizeof (x.b))
    FAIL ();
  if (__builtin_object_size (&x.b.a, 1) != sizeof (x.b.a))
    FAIL ();
  if (__builtin_object_size (&x.b.a[0], 1) != sizeof (x.b.a))
    FAIL ();
  if (__builtin_object_size (&x.b.a[3], 1) != sizeof (x.b.a) - 3)
    FAIL ();
  if (__builtin_object_size (&x.b.b, 1) != sizeof (x.b.b))
    FAIL ();
  if (__builtin_object_size (&x.b.c, 1) != sizeof (x.b.c))
    FAIL ();
  if (__builtin_object_size (&x.b.c[0], 1) != sizeof (x.b.c))
    FAIL ();
  if (__builtin_object_size (&x.b.c[3], 1) != sizeof (x.b.c) - 3)
    FAIL ();
  if (__builtin_object_size (&x.c, 1) != sizeof (x.c))
    FAIL ();
  if (__builtin_object_size (&x.c[0], 1) != sizeof (x.c))
    FAIL ();
  if (__builtin_object_size (&x.c[1], 1) != sizeof (x.c) - 1)
    FAIL ();
  if (__builtin_object_size (&x.d, 1) != sizeof (x.d))
    FAIL ();
  if (__builtin_object_size (&x.e, 1) != sizeof (x.e))
    FAIL ();
  if (__builtin_object_size (&x.f, 1) != sizeof (x.f))
    FAIL ();
  dp = &__real__ x.f;
  if (__builtin_object_size (dp, 1) != sizeof (x.f) / 2)
    FAIL ();
  dp = &__imag__ x.f;
  if (__builtin_object_size (dp, 1) != sizeof (x.f) / 2)
    FAIL ();
  dp = &y;
  if (__builtin_object_size (dp, 1) != sizeof (y))
    FAIL ();
  if (__builtin_object_size (&z, 1) != sizeof (z))
      FAIL ();
  dp = &__real__ z;
  if (__builtin_object_size (dp, 1) != sizeof (z) / 2)
    FAIL ();
  dp = &__imag__ z;
  if (__builtin_object_size (dp, 1) != sizeof (z) / 2)
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
      if (__builtin_object_size (p, 1) != (size_t) -1)
	FAIL ();
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
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 1) != sizeof (t.buf) - 8 - 4 * x)
    FAIL ();
#else
  if (__builtin_object_size (p, 1) != sizeof (t.buf) - 8)
    FAIL ();
#endif
  memset (p, ' ', sizeof (t.buf) - 8 - 4 * 4);
}

void
__attribute__ ((noinline))
test6 (void)
{
  char buf[64];
  struct T { char buf[64]; char buf2[64]; } t;
  char *p = &buf[64], *q = &t.buf[64];

  if (__builtin_object_size (p + 64, 1) != 0)
    FAIL ();
  if (__builtin_object_size (q + 0, 1) != 0)
    FAIL ();
  if (__builtin_object_size (q + 64, 1) != 0)
    FAIL ();
}

void
__attribute__ ((noinline))
test7 (void)
{
  struct T { char buf[10]; char buf2[10]; } t;
  char *p = &t.buf2[-4];
  char *q = &t.buf2[0];
  if (__builtin_object_size (p, 1) != 0)
    FAIL ();
  if (__builtin_object_size (q, 1) != sizeof (t.buf2))
    FAIL ();
  q = &t.buf[10];
  if (__builtin_object_size (q, 1) != 0)
    FAIL ();
  q = &t.buf[11];
  if (__builtin_object_size (q, 1) != 0)
    FAIL ();
  p = &t.buf[-4];
  if (__builtin_object_size (p, 1) != 0)
    FAIL ();
}

void
__attribute__ ((noinline))
test8 (unsigned cond)
{
  char *buf2 = malloc (10);
  char *p;

  if (cond)
    p = &buf2[8];
  else
    p = &buf2[4];

#ifdef __builtin_object_size
  if (__builtin_object_size (&p[-4], 1) != (cond ? 6 : 10))
    FAIL ();
#else
  if (__builtin_object_size (&p[-4], 1) != 10)
    FAIL ();
#endif

  for (unsigned i = cond; i > 0; i--)
    p--;

#ifdef __builtin_object_size
  if (__builtin_object_size (p, 1) != ((cond ? 2 : 6) + cond))
    FAIL ();
#else
  if (__builtin_object_size (p, 1) != 10)
    FAIL ();
#endif

  p = &y.c[8];
  for (unsigned i = cond; i > 0; i--)
    p--;

#ifdef __builtin_object_size
  if (__builtin_object_size (p, 1) != sizeof (y.c) - 8 + cond)
    FAIL ();
#else
  if (__builtin_object_size (p, 1) != sizeof (y.c))
    FAIL ();
#endif
}

/* Tests for strdup/strndup.  */
size_t
__attribute__ ((noinline))
test9 (void)
{
  const char *ptr = "abcdefghijklmnopqrstuvwxyz";
  char *res = strndup (ptr, 21);
  if (__builtin_object_size (res, 1) != 22)
    FAIL ();

  free (res);

  res = strndup (ptr, 32);
  if (__builtin_object_size (res, 1) != 27)
    FAIL ();

  free (res);

  res = strdup (ptr);
  if (__builtin_object_size (res, 1) != 27)
    FAIL ();

  free (res);

  char *ptr2 = malloc (64);
  strcpy (ptr2, ptr);

  res = strndup (ptr2, 21);
  if (__builtin_object_size (res, 1) != 22)
    FAIL ();

  free (res);

  res = strndup (ptr2, 32);
  if (__builtin_object_size (res, 1) != 33)
    FAIL ();

  free (res);

  res = strndup (ptr2, 128);
  if (__builtin_object_size (res, 1) != 64)
    FAIL ();

  free (res);

  res = strdup (ptr2);
#ifdef __builtin_object_size
  if (__builtin_object_size (res, 1) != 27)
#else
  if (__builtin_object_size (res, 1) != (size_t) -1)
#endif
    FAIL ();

  free (res);
  free (ptr2);

  ptr = "abcd\0efghijklmnopqrstuvwxyz";
  res = strdup (ptr);
  if (__builtin_object_size (res, 1) != 5)
    FAIL ();
  free (res);

  res = strndup (ptr, 24);
  if (__builtin_object_size (res, 1) != 5)
    FAIL ();
  free (res);

  res = strndup (ptr, 2);
  if (__builtin_object_size (res, 1) != 3)
    FAIL ();
  free (res);

  res = strdup (&ptr[4]);
  if (__builtin_object_size (res, 1) != 1)
    FAIL ();
  free (res);

  res = strndup (&ptr[4], 4);
  if (__builtin_object_size (res, 1) != 1)
    FAIL ();
  free (res);

  res = strndup (&ptr[4], 1);
  if (__builtin_object_size (res, 1) != 1)
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
  test6 ();
  test7 ();
  test8 (1);
  test9 ();
  DONE ();
}
