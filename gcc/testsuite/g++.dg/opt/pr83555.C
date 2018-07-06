// PR c++/83555
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized -fdelete-null-pointer-checks" }

struct A { int a; };
struct B { int b; };
struct C : A, B { int c; };

C *
foo (B *b)
{
  return &static_cast<C &>(*b);
}

// { dg-final { scan-tree-dump-not "if \\(b_\[0-9]*\\(D\\) .= 0" "optimized" } }
