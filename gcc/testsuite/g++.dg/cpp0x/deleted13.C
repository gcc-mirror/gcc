// PR c++/79519
// { dg-do compile { target c++11 } }

struct A
{
  template<typename> void foo();
};

struct B
{
  template<typename> friend void A::foo() = delete; // { dg-error "" }
};
