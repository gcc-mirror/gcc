// PR c++/71446
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct T { void *a; int b; };
struct U { int a; union { int b; union { long c; short d; }; }; int e; };
void bar (T);
void baz (U);

void
foo ()
{
  bar ({.b = 1});
  baz ({.c = 5L, .e = 6});
}
