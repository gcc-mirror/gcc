// PR c++/66383
// { dg-do compile { target c++11 } }

namespace N1 {
  struct B;

  struct A
  {
    B* b;
    A(B* b);
  };

  struct B
  {
    A a{ this };
  };

  A::A(B* b): b{ b } {}

  void foo()
  {
    auto b = B{};
  }
}

namespace N2 {
  struct B;

  struct A
  {
    B* b;
  };

  struct B
  {
    A a{ this };
  };

  void foo()
  {
    auto b = B{};
  }
}
