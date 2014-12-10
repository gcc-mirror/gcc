/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

int c;

__attribute__((noinline, noclone)) void
f1 (int *a, char *b)
{
  __builtin_memcpy (a, b, sizeof (*a));
}

__attribute__((noinline, noclone)) void
f2 (int *a, char *b)
{
  __builtin_memcpy (b, a, sizeof (*a));
}

__attribute__((noinline, noclone)) void
f3 (char *b)
{
  __builtin_memcpy (&c, b, sizeof (c));
}

__attribute__((noinline, noclone)) void
f4 (char *b)
{
  __builtin_memcpy (b, &c, sizeof (c));
}

struct T
{
  char a;
  short b;
  int c;
  long d;
  long long e;
  short f;
  float g;
  double h;
  long double i;
} __attribute__((packed));

__attribute__((noinline, noclone)) int
f5 (struct T *p)
{
  return p->a + p->b + p->c + p->d + p->e + p->f + p->g + p->h + p->i;
}

int
main ()
{
  struct S { int a; char b[sizeof (int) + 1]; } s;
  s.a = 6;
  f2 (&s.a, &s.b[1]);
  f1 (&s.a, &s.b[1]);
  c = s.a + 1;
  f4 (&s.b[1]);
  f3 (&s.b[1]);
  if (c != 7 || s.a != 6)
    __builtin_abort ();
  struct U { long long a; long double b; char c; struct T d; } u;
  __builtin_memset (&u, 0, sizeof (u));
  if (f5 (&u.d) != 0)
    __builtin_abort ();
  return 0;
}
