/* { dg-do run } */
/* { dg-options "-O2 -fno-exceptions" } */

struct S
{
  inline int fn1 () const { return s; }
  __attribute__ ((noinline, noclone)) S *fn2 (int);
  __attribute__ ((noinline, noclone)) void fn3 ();
  __attribute__ ((noinline, noclone)) static S *fn4 (int);
  S (int i) : s (i) {}
  int s;
};

int a = 0;
S *b = 0;

S *
S::fn2 (int i)
{
  a++;
  if (a == 1)
    return b;
  if (a > 3)
    __builtin_abort ();
  b = this;
  return new S (i + s);
}

S *
S::fn4 (int i)
{
  b = new S (i);
  return b;
}

void
S::fn3 ()
{
  delete this;
}

void
foo ()
{
  S *c = S::fn4 (20);
  for (int i = 0; i < 2;)
    {
      S *d = c->fn2 (c->fn1 () + 10);
      if (d != c)
{
  c->fn3 ();
  c = d;
  ++i;
}
    }
  c->fn3 ();
}

int
main ()
{
  foo ();
}
