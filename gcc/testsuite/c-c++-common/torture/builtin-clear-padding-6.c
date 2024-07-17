/* PR middle-end/115527 */
/* { dg-do run } */

struct T { struct S { double a; signed char b; long c; } d[3]; int e; } t1, t2;

__attribute__((noipa)) void
foo (struct T *t)
{
  for (int i = 0; i < 3; ++i)
    {
      t->d[i].a = 1. + 3 * i;
      t->d[i].b = 2 + 3 * i;
      t->d[i].c = 3 + 3 * i;
    }
  t->e = 10;
}

int
main ()
{
  __builtin_memset (&t2, -1, sizeof (t2));
  foo (&t1);
  foo (&t2);
  __builtin_clear_padding (&t2);
  if (__builtin_memcmp (&t1, &t2, sizeof (t1)))
    __builtin_abort ();
  return 0;
}
