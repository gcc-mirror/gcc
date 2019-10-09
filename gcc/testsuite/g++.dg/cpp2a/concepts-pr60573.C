// PR c++/60573
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct A
{
  struct B
  {
    void foo(auto);
  };

  void B::foo(auto) {}  // { dg-error "8:cannot define" }

  struct X
  {
    struct Y
    {
      struct Z
      {
        void foo(auto);
      };
    };

    void Y::Z::foo(auto) {}  // { dg-error "10:cannot define" }
  };

  void X::Y::Z::foo(auto) {}  // { dg-error "8:cannot define" }
};
