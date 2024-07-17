/* PR libstdc++/88101 */
/* { dg-do run } */

struct S { char a; short b; char c; } s1[24], s2[24];
struct T { char a; long long b; char c; struct S d[3]; long long e; char f; } t1, t2;
struct U { char a; long long b; char c; struct S d[25]; long long e; char f; } u1, u2;

__attribute__((noipa)) void
foo (struct S *s, struct T *t, struct U *u)
{
  int i;
  t->a = -1; t->b = -1; t->c = -1; t->e = -1; t->f = -1;
  u->a = -1; u->b = -1; u->c = -1; u->e = -1; u->f = -1;
  for (i = 0; i < 24; i++)
    {
      s[i].a = -1;
      s[i].b = -1;
      s[i].c = -1;
    }
  for (i = 0; i < 3; i++)
    {
      t->d[i].a = -1;
      t->d[i].b = -1;
      t->d[i].c = -1;
    }
  for (i = 0; i < 25; i++)
    {
      u->d[i].a = -1;
      u->d[i].b = -1;
      u->d[i].c = -1;
    }
}

int
main ()
{
  __builtin_memset (&s2, -1, sizeof (s2));
  __builtin_memset (&t2, -1, sizeof (t2));
  __builtin_memset (&u2, -1, sizeof (u2));
  foo (&s1[0], &t1, &u1);
  foo (&s2[0], &t2, &u2);
  __builtin_clear_padding (&s2);
  __builtin_clear_padding (&t2);
  __builtin_clear_padding (&u2);
  if (__builtin_memcmp (&s1, &s2, sizeof (s1))
      || __builtin_memcmp (&t1, &t2, sizeof (t1))
      || __builtin_memcmp (&u1, &u2, sizeof (u1)))
    __builtin_abort ();
  return 0;
}
