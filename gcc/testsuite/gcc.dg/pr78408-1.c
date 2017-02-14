/* PR c/78408 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -fdump-tree-fab1-details" } */
/* { dg-final { scan-tree-dump-times "after previous" 17 "fab1" } } */

struct S { char a[32]; };
struct T { char a[65536]; };
void bar (int, struct S *, struct S *, struct T *, struct T *);
void baz (char *, char *);

void
f1 (void)
{
  struct S a, b;
  struct T c, d;
  a = b = (struct S) {};
  c = d = (struct T) {};
  bar (1, &a, &b, &c, &d);
}

void
f2 (void)
{
  struct S a, b;
  struct T c, d;
  b = (struct S) {};
  a = b;
  d = (struct T) {};
  c = d;
  bar (2, &a, &b, &c, &d);
}

void
f3 (void)
{
  struct S a, b;
  struct T c, d;
  __builtin_memset (&b, 0, sizeof (b));
  a = b;
  __builtin_memset (&d, 0, sizeof (d));
  c = d;
  bar (3, &a, &b, &c, &d);
}


void
f4 (void)
{
  struct S a, b;
  struct T c, d;
  b = (struct S) {};
  __builtin_memcpy (&a, &b, sizeof (b));
  d = (struct T) {};
  __builtin_memcpy (&c, &d, sizeof (d));
  bar (4, &a, &b, &c, &d);
}

void
f5 (void)
{
  struct S a, b;
  struct T c, d;
  __builtin_memset (&b, 0, sizeof (b));
  __builtin_memcpy (&a, &b, sizeof (b));
  __builtin_memset (&d, 0, sizeof (d));
  __builtin_memcpy (&c, &d, sizeof (d));
  bar (5, &a, &b, &c, &d);
}

void
f6 (void)
{
  struct S a, b, e, g;
  struct T c, d, f, h;
  g = e = a = b = (struct S) {};
  h = f = c = d = (struct T) {};
  bar (6, &a, &b, &c, &d);
  bar (6, &e, &g, &f, &h);
}

void
f7 (void)
{
  char a[64], b[64];
  __builtin_memset (a + 13, 2, 27);
  __builtin_memcpy (b + 4, a + 17, 23);
  baz (a, b);
}
