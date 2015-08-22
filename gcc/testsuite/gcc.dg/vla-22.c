/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target alloca } */

extern void bar (int *);

static inline __attribute__((always_inline))
int
foo (int i)
{
  struct S {
    int ar[1][i];
  } s;

  s.ar[0][0] = 0;
  bar (&s.ar[0][0]);
}

void
baz (int i)
{
  foo (i + 2);
}
