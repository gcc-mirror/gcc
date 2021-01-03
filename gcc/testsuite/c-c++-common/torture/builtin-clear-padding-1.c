/* PR libstdc++/88101 */

int i1, i2;
long double l1, l2;
struct S { char a; short b; char c; int d; char e; long long f; char g; long double h; } s1, s2;
struct T { int a; struct S b[3]; int c; } t1, t2;
struct U { int a : 3; int : 2; int b : 15; int : 14; int c : 1; int : 0; int : 3; int d : 2; int : 3; int e : 13; int : 3; signed char f; } u1, u2;

__attribute__((noipa)) void
foo (int *i, long double *l, struct S *s, struct T *t, struct U *u)
{
  *i = 123;
  *l = -123.456L;
  s->a = 1; s->b = 2; s->c = 3; s->d = 4; s->e = 5; s->f = 6; s->g = 7; s->h = 18.52L;
  t->a = 8; t->c = 9;
  t->b[0].a = 11; t->b[0].b = 12; t->b[0].c = 13; t->b[0].d = 14;
  t->b[0].e = 15; t->b[0].f = 16; t->b[0].g = 17; t->b[0].h = 18.26L;
  t->b[1].a = 21; t->b[1].b = 22; t->b[1].c = 23; t->b[1].d = 24;
  t->b[1].e = 25; t->b[1].f = 26; t->b[1].g = 27; t->b[1].h = 28.26L;
  t->b[2].a = 31; t->b[2].b = 32; t->b[2].c = 33; t->b[2].d = 34;
  t->b[2].e = 35; t->b[2].f = 36; t->b[2].g = 37; t->b[2].h = 38.26L;
  u->a = -1; u->b = -1; u->c = -1; u->d = -1; u->e = -1; u->f = -1;
}

int
main ()
{
  __builtin_memset (&i2, -1, sizeof (i2));
  __builtin_memset (&l2, -1, sizeof (i2));
  __builtin_memset (&s2, -1, sizeof (s2));
  __builtin_memset (&t2, -1, sizeof (t2));
  __builtin_memset (&u2, -1, sizeof (u2));
  foo (&i1, &l1, &s1, &t1, &u1);
  foo (&i2, &l2, &s2, &t2, &u2);
  __builtin_clear_padding (&i2);
  __builtin_clear_padding (&l2);
  __builtin_clear_padding (&s2);
  __builtin_clear_padding (&t2);
  __builtin_clear_padding (&u2);
  if (__builtin_memcmp (&i1, &i2, sizeof (i1))
      || __builtin_memcmp (&l1, &l2, sizeof (l1))
      || __builtin_memcmp (&s1, &s2, sizeof (s1))
      || __builtin_memcmp (&t1, &t2, sizeof (t1))
      || __builtin_memcmp (&u1, &u2, sizeof (u1)))
    __builtin_abort ();
  return 0;
}
