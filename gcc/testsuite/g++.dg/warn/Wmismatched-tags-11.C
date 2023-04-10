// PR c++/106259
// { dg-do compile }
// { dg-options "-Wmismatched-tags" }

template<typename T> struct A {
  template<typename U>
  struct W { };
};

template<>
struct A<char> {
  template<typename U>
  class W { };
};

void
g ()
{
  struct A<char>::W<int> w1; // { dg-warning "mismatched" }
  struct A<int>::W<int> w2;
  class A<char>::W<int> w3;
  class A<int>::W<int> w4; // { dg-warning "mismatched" }
}
