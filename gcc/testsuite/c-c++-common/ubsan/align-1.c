/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

struct S { int a; char b; long long c; short d[10]; };
struct T { char a; long long b; };
struct U { char a; int b; int c; long long d; struct S e; struct T f; };
struct V { long long a; struct S b; struct T c; struct U u; } v;

__attribute__((noinline, noclone)) void
f1 (int *p, int *q, char *r, long long *s)
{
  *p = *q + *r + *s;
}


__attribute__((noinline, noclone)) int
f2 (struct S *p)
{
  return p->a;
}

__attribute__((noinline, noclone)) long long
f3 (struct S *p, int i)
{
  return p->c + p->d[1] + p->d[i];
}

__attribute__((noinline, noclone)) long long
f4 (long long *p)
{
  return *p;
}

int
main ()
{
  f1 (&v.u.b, &v.u.c, &v.u.a, &v.u.d);
  if (f2 (&v.u.e) + f3 (&v.u.e, 4) + f4 (&v.u.f.b) != 0)
    __builtin_abort ();
  return 0;
}
