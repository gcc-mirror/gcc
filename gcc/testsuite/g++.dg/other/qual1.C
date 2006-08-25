// PR c++/28257
// { dg-do compile }

struct A
{
  int i;
  void foo()
  {
    int A::i = i;  // { dg-error "qualified" }
  }
};
