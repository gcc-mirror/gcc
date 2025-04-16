// PR tree-optimization/119614
// { dg-lto-do link }
// { dg-lto-options { { -O2 -fPIC -flto -flto-partition=max } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-require-effective-target musttail }
// { dg-extra-ld-options "-shared" }

struct S {} b;
char *foo ();
int e, g;
void bar ();
void corge (S);

[[gnu::noinline]] static char *
baz ()
{
  bar ();
  return 0;
}

const char *
qux ()
{
  if (e)
    {
      S a = b;
      corge (a);
      if (g)
        return 0;
      [[gnu::musttail]] return baz ();
    }
  return foo ();
}
