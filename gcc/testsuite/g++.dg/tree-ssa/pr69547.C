// { dg-do compile }
// { dg-options "-O2 -fdump-tree-cddce1" }

struct A { A () { } };

void foo (void*, int);

void bar ()
{
  enum { N = 64 };
  A a [N];
  foo (&a, N);
}

// { dg-final { scan-tree-dump-not "if" "cddce1" } }
