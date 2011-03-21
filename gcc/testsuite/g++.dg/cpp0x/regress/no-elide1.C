// PR c++/47503
// { dg-options "-std=c++0x -fno-elide-constructors" }

struct A
{
  int i;
  A ();
};

struct B
{
  A a;
  B (A &aa) : a (aa) { }
};
