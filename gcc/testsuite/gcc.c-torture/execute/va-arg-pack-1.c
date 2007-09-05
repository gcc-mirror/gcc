/* __builtin_va_arg_pack () builtin tests.  */

#include <stdarg.h>

extern void abort (void);

int v1 = 8;
long int v2 = 3;
void *v3 = (void *) &v2;
struct A { char c[16]; } v4 = { "foo" };
long double v5 = 40;
char seen[20];
int cnt;

__attribute__ ((noinline)) int
foo1 (int x, int y, ...)
{
  int i;
  long int l;
  void *v;
  struct A a;
  long double ld;
  va_list ap;

  va_start (ap, y);
  if (x < 0 || x >= 20 || seen[x])
    abort ();
  seen[x] = ++cnt;
  if (y != 6)
    abort ();
  i = va_arg (ap, int);
  if (i != 5)
    abort ();
  switch (x)
    {
    case 0:
      i = va_arg (ap, int);
      if (i != 9 || v1 != 9)
	abort ();
      a = va_arg (ap, struct A);
      if (__builtin_memcmp (a.c, v4.c, sizeof (a.c)) != 0)
	abort ();
      v = (void *) va_arg (ap, struct A *);
      if (v != (void *) &v4)
	abort ();
      l = va_arg (ap, long int);
      if (l != 3 || v2 != 4)
	abort ();
      break;
    case 1:
      ld = va_arg (ap, long double);
      if (ld != 41 || v5 != ld)
	abort ();
      i = va_arg (ap, int);
      if (i != 8)
	abort ();
      v = va_arg (ap, void *);
      if (v != &v2)
	abort ();
      break;
    case 2:
      break;
    default:
      abort ();
    }
  va_end (ap);
  return x;
}

__attribute__ ((noinline)) int
foo2 (int x, int y, ...)
{
  long long int ll;
  void *v;
  struct A a, b;
  long double ld;
  va_list ap;

  va_start (ap, y);
  if (x < 0 || x >= 20 || seen[x])
    abort ();
  seen[x] = ++cnt | 64;
  if (y != 10)
    abort ();
  switch (x)
    {
    case 11:
      break;
    case 12:
      ld = va_arg (ap, long double);
      if (ld != 41 || v5 != 40)
	abort ();
      a = va_arg (ap, struct A);
      if (__builtin_memcmp (a.c, v4.c, sizeof (a.c)) != 0)
	abort ();
      b = va_arg (ap, struct A);
      if (__builtin_memcmp (b.c, v4.c, sizeof (b.c)) != 0)
	abort ();
      v = va_arg (ap, void *);
      if (v != &v2)
	abort ();
      ll = va_arg (ap, long long int);
      if (ll != 16LL)
	abort ();
      break;
    case 2:
      break;
    default:
      abort ();
    }
  va_end (ap);
  return x + 8;
}

__attribute__ ((noinline)) int
foo3 (void)
{
  return 6;
}

extern inline __attribute__ ((always_inline, gnu_inline)) int
bar (int x, ...)
{
  if (x < 10)
    return foo1 (x, foo3 (), 5, __builtin_va_arg_pack ());
  return foo2 (x, foo3 () + 4, __builtin_va_arg_pack ());
}

int
main (void)
{
  if (bar (0, ++v1, v4, &v4, v2++) != 0)
    abort ();
  if (bar (1, ++v5, 8, v3) != 1)
    abort ();
  if (bar (2) != 2)
    abort ();
  if (bar (v1 + 2) != 19)
    abort ();
  if (bar (v1 + 3, v5--, v4, v4, v3, 16LL) != 20)
    abort ();
  return 0;
}
