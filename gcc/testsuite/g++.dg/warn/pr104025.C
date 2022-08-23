// PR c++/104025
// { dg-do compile }
// { dg-options "-Wmissing-template-keyword -fcompare-debug" }

void bar (int);

struct S { int i; };

template <class C>
struct T
{
  int m;
  C c;
  void foo ()
  {
    bar (c.i < m);
  }
};

template void T<S>::foo ();
