// PR c++/71638
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

struct A {
  struct {
    int i;
    int &j = i;
  } b;
  int a = b.j;
};

void bar (A);

void
foo ()
{
  bar (A{});
}
