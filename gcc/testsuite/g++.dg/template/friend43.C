// PR c++/28111
// { dg-do compile }

template<typename> void foo();

template<typename T> struct A
{
  friend void foo<>(typename T::X);  // { dg-error "not a class" }
};

A<int> a;
