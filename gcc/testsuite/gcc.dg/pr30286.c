/* PR middle-end/30286 */
/* { dg-do run } */
/* { dg-options "-O2 -ftrapv" } */
/* { dg-require-effective-target trapping } */

extern void abort (void);
struct S { struct S *s; };
struct T { struct S *t[25]; };

void
__attribute__((noinline))
foo (int i, struct T *x, struct S *y)
{
  int j;
  for (j = 14; j > i; j--)
    x->t[j] = y->s;
}

int
main (void)
{
  struct S s;
  struct T t;
  int i;

  s.s = &s;
  __builtin_memset (&t, 0, sizeof (t));
  foo (6, &t, &s);
  for (i = 0; i < 25; i++)
    if (t.t[i] != ((i > 6 && i <= 14) ? &s : (struct S *) 0))
      abort ();
  __builtin_memset (&t, 0, sizeof (t));
  foo (-1, &t, &s);
  for (i = 0; i < 25; i++)
    if (t.t[i] != ((i >= 0 && i <= 14) ? &s : (struct S *) 0))
      abort ();
  return 0;
}
