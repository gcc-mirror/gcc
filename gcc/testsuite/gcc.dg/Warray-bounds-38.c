/* PR middle-end/88273 - bogus warning: 'memcpy' offset [-527, -529]
   is out of the bounds [0, 16]
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */

typedef __SIZE_TYPE__ size_t;

void *q;

size_t x, y;

inline void f (char *p, int i, size_t j)
{
  size_t n = y ? y : j;

  p += x - i;

  __builtin_memcpy (q, p, n);   /* { dg-bogus "bounds" } */

  x = n;
}

void g (void)
{
  struct { char a[16]; } s;

  f (q, 0, sizeof s);

  f (s.a, 33 * sizeof s, 1);
}
