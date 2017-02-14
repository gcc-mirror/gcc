/* PR middle-end/22141 */

extern void abort (void);

struct S
{
  struct T
    {
      char a;
      char b;
      char c;
      char d;
    } t;
} u;

struct U
{
  struct S s[4];
};

void __attribute__((noinline))
c1 (struct T *p)
{
  if (p->a != 1 || p->b != 2 || p->c != 3 || p->d != 4)
    abort ();
  __builtin_memset (p, 0xaa, sizeof (*p));
}

void __attribute__((noinline))
c2 (struct S *p)
{
  c1 (&p->t);
}

void __attribute__((noinline))
c3 (struct U *p)
{
  c2 (&p->s[2]);
}

void __attribute__((noinline))
f1 (void)
{
  u = (struct S) { { 1, 2, 3, 4 } };
}

void __attribute__((noinline))
f2 (void)
{
  u.t.a = 1;
  u.t.b = 2;
  u.t.c = 3;
  u.t.d = 4;
}

void __attribute__((noinline))
f3 (void)
{
  u.t.d = 4;
  u.t.b = 2;
  u.t.a = 1;
  u.t.c = 3;
}

void __attribute__((noinline))
f4 (void)
{
  struct S v;
  v.t.a = 1;
  v.t.b = 2;
  v.t.c = 3;
  v.t.d = 4;
  c2 (&v);
}

void __attribute__((noinline))
f5 (struct S *p)
{
  p->t.a = 1;
  p->t.c = 3;
  p->t.d = 4;
  p->t.b = 2;
}

void __attribute__((noinline))
f6 (void)
{
  struct U v;
  v.s[2].t.a = 1;
  v.s[2].t.b = 2;
  v.s[2].t.c = 3;
  v.s[2].t.d = 4;
  c3 (&v);
}

void __attribute__((noinline))
f7 (struct U *p)
{
  p->s[2].t.a = 1;
  p->s[2].t.c = 3;
  p->s[2].t.d = 4;
  p->s[2].t.b = 2;
}

int
main (void)
{
  struct U w;
  f1 ();
  c2 (&u);
  f2 ();
  c1 (&u.t);
  f3 ();
  c2 (&u);
  f4 ();
  f5 (&u);
  c2 (&u);
  f6 ();
  f7 (&w);
  c3 (&w);
  return 0;
}
