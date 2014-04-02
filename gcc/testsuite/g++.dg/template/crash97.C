// PR c++/34272

template<typename> struct A {};

template<typename> struct A<int> // { dg-error "not deducible|template\\-parameter" }
{
  template<int> void foo();
};

void bar()
{
  A<int> a; // { dg-error "incomplete type" }
  a.foo<0>(); // { dg-error "expected" }
}
