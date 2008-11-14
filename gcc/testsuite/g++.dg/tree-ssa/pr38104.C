// PR tree-optimization/38104
// { dg-do compile }
// { dg-options "-O3" }

struct S { int foo; };

void f0 ();

void
f1 (struct S s)
{
  f0 ();
}

void
f2 ()
{
  f1 (*(struct S *) (0));
}
