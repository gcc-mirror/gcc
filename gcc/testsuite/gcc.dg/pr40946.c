/* PR middle-end/40946 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct A
{
  unsigned char a1[8];
  unsigned short a2, a3, a4, a5, a6, a7;
};

int bar (void *, const char *, int);
char *foo (void *, int);

char *
baz (const char *x, const char *y)
{
  int a = sizeof (struct A) + 2 * (__builtin_strlen (y)
				   + __builtin_strlen (x) + 24) + 16;
  struct A *b = __builtin_malloc (a);
  char *c;
  int d;

  b->a4 = sizeof (struct A);
  c = ((char *) b) + b->a4;
  d = ((char *) b) + a - c;
  b->a2 = b->a3 = (unsigned short) bar (c, y, d); /* { dg-bogus "array subscript is above array bounds" } */
  c += b->a2;
  d = ((char *) b) + a - c;
  b->a7 = b->a4 + b->a2;
  b->a5 = b->a6 = (unsigned short) bar (c, x, d);
  c = foo (b, a);
  return c;
}
