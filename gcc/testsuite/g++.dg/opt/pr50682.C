// PR tree-optimization/50682
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -ftracer -fno-tree-ccp -fno-tree-copy-prop -fno-tree-dce" }

void foo () __attribute__ ((__noreturn__));
int baz ();

const int &
bar (const int &x, const int &y)
{
  if (x >= y)
    return y;
  return x;
}

int a, b;

struct S
{
  ~S ();
  bool m ()
  {
    int l = bar (a, b);
    int r = baz ();
    if (r)
        r = l;
      return r;
  }
};

void
test ()
{
  S s;
  if (!s.m ())
    foo ();
  if (!s.m ())
    foo ();
}
