/* Copyright (C) 2003 Free Software Foundation.

   Test memcpy and memset in presence of redirect.  */

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern void *memcpy (void *, const void *, size_t)
  __asm ("my_memcpy");
extern void bcopy (const void *, void *, size_t)
  __asm ("my_bcopy");
extern void *memset (void *, int, size_t)
  __asm ("my_memset");
extern void bzero (void *, size_t)
  __asm ("my_bzero");
extern int memcmp (const void *, const void *, size_t);

struct A { char c[32]; } a = { "foobar" };
char x[64] = "foobar", y[64];
int i = 39, j = 6, k = 4;

int
main (void)
{
  struct A b = a;
  struct A c = { { 'x' } };

  if (memcmp (b.c, x, 32) || c.c[0] != 'x' || memcmp (c.c + 1, x + 32, 31))
    abort ();
  if (__builtin_memcpy (y, x, i) != y || memcmp (x, y, 64))
    abort ();
  if (memcpy (y + 6, x, j) != y + 6
      || memcmp (x, y, 6) || memcmp (x, y + 6, 58))
    abort ();
  if (__builtin_memset (y + 2, 'X', k) != y + 2
      || memcmp (y, "foXXXXfoobar", 13))
    abort ();
  bcopy (y + 1, y + 2, 6);
  if (memcmp (y, "fooXXXXfobar", 13))
    abort ();
  __builtin_bzero (y + 4, 2);
  if (memcmp (y, "fooX\0\0Xfobar", 13))
    abort ();

  return 0;
}

/* There should be no calls to real memcpy, memset, bcopy or bzero.  */
static void *real_memcpy (void *, const void *, size_t)
  __asm ("memcpy");
static void real_bcopy (const void *, void *, size_t)
  __asm ("bcopy");
static void *real_memset (void *, int, size_t)
  __asm ("memset");
static void real_bzero (void *, size_t)
  __asm ("bzero");

__attribute__ ((noinline))
static void *
real_memcpy (void *d, const void *s, size_t n)
{
  abort ();
}

__attribute__ ((noinline))
static void
real_bcopy (const void *s, void *d, size_t n)
{
  abort ();
}

__attribute__ ((noinline))
static void *
real_memset (void *d, int c, size_t n)
{
  abort ();
}

__attribute__ ((noinline))
static void
real_bzero (void *d, size_t n)
{
  abort ();
}

__attribute__ ((noinline))
void *
memcpy (void *d, const void *s, size_t n)
{
  char *dst = (char *) d;
  const char *src = (const char *) s;
  while (n--)
    *dst++ = *src++;
  return (char *) d;
}

__attribute__ ((noinline))
void
bcopy (const void *s, void *d, size_t n)
{
  char *dst = (char *) d;
  const char *src = (const char *) s;
  if (src >= dst)
    while (n--)
      *dst++ = *src++;
  else
    {
      dst += n;
      src += n;
      while (n--)
        *--dst = *--src;
    }
}

__attribute__ ((noinline))
void *
memset (void *d, int c, size_t n)
{
  char *dst = (char *) d;
  while (n--)
    *dst++ = c;
  return (char *) d;
}

__attribute__ ((noinline))
void
bzero (void *d, size_t n)
{
  char *dst = (char *) d;
  while (n--)
    *dst++ = '\0';
}
