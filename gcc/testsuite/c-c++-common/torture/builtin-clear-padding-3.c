/* PR libstdc++/88101 */
/* { dg-do run } */

union V { char a; signed char b; unsigned char c; };
struct T { char a; int b; union V c; };
union U { int a; long double b; struct T c; };
struct S { char a; union U b; long long c; char d; } s1, s2;

__attribute__((noipa)) void
foo (struct S *s, int x)
{
  s->a = -1; s->c = -1; s->d = -1;
  switch (x)
    {
    case 0:
      s->b.a = -1;
      break;
    case 1:
      s->b.b = -12345.25L;
      break;
    case 2:
      s->b.c.a = -1;
      s->b.c.b = -1;
      s->b.c.c.b = -1;
      break;
    }
}

int
main ()
{
  __builtin_memset (&s1, 0, sizeof (s1));
  __builtin_memset (&s2, -1, sizeof (s2));
  foo (&s1, 0);
  foo (&s2, 0);
  __builtin_clear_padding (&s2);
  if (s2.b.a != -1)
    __builtin_abort ();
  __builtin_clear_padding (&s2.b.a);
  __builtin_memset (&s2.b.a + 1, 0, sizeof (union U) - sizeof (s2.b.a));
  if (__builtin_memcmp (&s1, &s2, sizeof (s1)))
    __builtin_abort ();
  __builtin_memset (&s1, 0, sizeof (s1));
  __builtin_memset (&s2, -1, sizeof (s2));
  foo (&s1, 1);
  foo (&s2, 1);
  __builtin_clear_padding (&s2);
  if (s2.b.b != -12345.25L)
    __builtin_abort ();
  __builtin_clear_padding (&s2.b.b);
  __builtin_memset (&s2.b.b + 1, 0, sizeof (union U) - sizeof (s2.b.b));
  if (__builtin_memcmp (&s1, &s2, sizeof (s1)))
    __builtin_abort ();
  __builtin_memset (&s1, 0, sizeof (s1));
  __builtin_memset (&s2, -1, sizeof (s2));
  foo (&s1, 2);
  foo (&s2, 2);
  __builtin_clear_padding (&s2);
  if (s2.b.c.a != (char) -1 || s2.b.c.b != -1 || s2.b.c.c.b != -1)
    __builtin_abort ();
  __builtin_clear_padding (&s2.b.c);
  __builtin_memset (&s2.b.c + 1, 0, sizeof (union U) - sizeof (s2.b.c));
  if (__builtin_memcmp (&s1, &s2, sizeof (s1)))
    __builtin_abort ();
  return 0;
}
