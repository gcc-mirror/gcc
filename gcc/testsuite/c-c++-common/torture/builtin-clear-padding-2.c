/* PR libstdc++/88101 */
/* { dg-do run } */
/* { dg-require-effective-target size20plus } */

typedef int T __attribute__((aligned (16384)));
struct S { char a; short b; long double c; T d; T e; long long f; };

__attribute__((noipa)) void
foo (struct S *s)
{
  s->a = -1; s->b = -1; s->c = -18.52L; s->d = -1; s->e = -1; s->f = -1;
}

int
main ()
{
  struct S s1, s2;
  __builtin_memset (&s1, 0, sizeof (s1));
  __builtin_memset (&s2, -1, sizeof (s2));
  foo (&s1);
  foo (&s2);
  __builtin_clear_padding (&s2);
  if (__builtin_memcmp (&s1, &s2, sizeof (s1)))
    __builtin_abort ();
  return 0;
}
