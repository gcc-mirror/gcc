/* PR libstdc++/88101 */
/* { dg-do run } */

struct S {} s1, s2;
struct T : public S { char a; short b; char c; } t1, t2;
struct U : public T { char d; long long e; char f; } u1, u2;

__attribute__((noipa)) void
foo (T *t, U *u)
{
  int i;
  t->a = -1; t->b = -1; t->c = -1;
  u->a = -1; u->b = -1; u->c = -1; u->d = -1; u->e = -1; u->f = -1;
}

int
main ()
{
  __builtin_memset (&s2, -1, sizeof (s2));
  __builtin_memset (&t2, -1, sizeof (t2));
  __builtin_memset (&u2, -1, sizeof (u2));
  foo (&t1, &u1);
  foo (&t2, &u2);
  __builtin_clear_padding (&s2);
  __builtin_clear_padding (&t2);
  __builtin_clear_padding (&u2);
  if (__builtin_memcmp (&s1, &s2, sizeof (s1))
      || __builtin_memcmp (&t1, &t2, sizeof (t1))
      || __builtin_memcmp (&u1, &u2, sizeof (u1)))
    __builtin_abort ();
  return 0;
}
