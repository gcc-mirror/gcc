// PR c++/60994
// { dg-do compile { target c++11 } }

struct B {
  enum struct A
  {
    n = 3
  };
};

struct C: B
{
  using B::A;

  A foo()
  {
    int A;
    return A::n;
  }
};
