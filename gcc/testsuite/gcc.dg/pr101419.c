/* PR tree-optimization/101419 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;
void baz (int, int) __attribute__((__warning__("detected overflow")));

union U {
  int i;
  char c;
};

static void
foo (union U *u)
{
  if (__builtin_object_size (&u->c, 1) < sizeof (u->c))
    baz (__builtin_object_size (&u->c, 1), sizeof (u->c));	/* { dg-bogus "detected overflow" } */
  __builtin_memset (&u->c, 0, sizeof (u->c));

  if (__builtin_object_size (&u->i, 1) < sizeof (u->i))
    baz (__builtin_object_size (&u->i, 1), sizeof (u->i));	/* { dg-bogus "detected overflow" } */
  __builtin_memset (&u->i, 0, sizeof (u->i));
}

void
bar (union U *u)
{
  int i, j;
  for (i = 0; i < 1; i++)
    {
      foo (u);
      for (j = 0; j < 2; j++)
        asm volatile ("");
    }
}

static void
qux (void *p, size_t q)
{
  if (__builtin_object_size (p, 1) < q)
    baz (__builtin_object_size (p, 1), q);			/* { dg-bogus "detected overflow" } */
  __builtin_memset (p, 0, q);
}

static void
corge (union U *u)
{
  qux (&u->c, sizeof (u->c));
  qux (&u->i, sizeof (u->i));
}

void
garply (union U *u)
{
  int i, j;
  for (i = 0; i < 1; i++)
    {
      corge (u);
      for (j = 0; j < 2; j++)
        asm volatile ("");
    }
}
