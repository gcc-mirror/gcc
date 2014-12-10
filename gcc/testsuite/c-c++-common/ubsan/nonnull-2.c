/* { dg-do run } */
/* { dg-shouldfail "ubsan" } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

int q, r;
void *a, *b, *c = (void *) &q, *d, *e, *f = (void *) &q, *g, *h;

__attribute__((returns_nonnull, nonnull (1, 3)))
void *
foo (void *p, void *q, void *r)
{
  a = p;
  b = r;
  return q;
}

int
bar (const void *a, const void *b)
{
  int c = *(const int *) a;
  int d = *(const int *) b;
  return c - d;
}

int
main ()
{
  asm volatile ("" : : : "memory");
  d = foo (c, b, c);
  e = foo (e, c, f);
  g = foo (c, f, g);
  __builtin_memset (d, '\0', q);
  return 0;
}

/* { dg-output "\.c:14:\[0-9]*:\[^\n\r]*null pointer returned from function declared to never return null" } */
