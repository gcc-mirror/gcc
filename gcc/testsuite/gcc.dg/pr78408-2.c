/* PR c/78408 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -fdump-tree-fab1-details" } */
/* { dg-final { scan-tree-dump-not "after previous" "fab1" } } */

struct S { char a[32]; };
struct T { char a[65536]; };
void bar (int, struct S *, struct S *, struct T *, struct T *);
void baz (char *, char *);

void
f1 (void)
{
  struct S a, b;
  struct T c, d;
  __builtin_memset (&b, 2, sizeof (b));
  a = b;
  __builtin_memset (&d, 3, sizeof (d));
  c = d;
  bar (3, &a, &b, &c, &d);
}

void
f2 (void)
{
  char a[64], b[64];
  __builtin_memset (a + 13, 2, 27);
  __builtin_memcpy (b + 4, a + 17, 24);
  baz (a, b);
}

void
f3 (void)
{
  char a[64], b[64];
  __builtin_memset (a + 13, 2, 27);
  __builtin_memcpy (b + 4, a + 12, 5);
  baz (a, b);
}
