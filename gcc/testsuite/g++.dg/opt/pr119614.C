// PR tree-optimization/119614
// { dg-do compile { target musttail } }
// { dg-options "-O2" }

struct S {} b;
char *foo ();
int e, g;
void bar ();
void corge (S);

[[gnu::noinline]] char *
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
