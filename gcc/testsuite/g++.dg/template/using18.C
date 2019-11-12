// PR c++/23211
// { dg-do compile }

template<class T>
struct foo 
{
  typedef int jtype;
  struct bar 
  {
    using typename foo<T>::jtype; // { dg-error "not a base type" }
    using foo<T>::jtype; // { dg-error "not a base type" }
  };
};

template <class T>
struct A : T
{
    using T::i;
    using typename T::type;
};

struct B1 {};
template <class T> struct B2 {};

template <class T>
struct C : B1, B2<T>
{
    using B1::x; // { dg-error "has not been declared" }
    using B2<T>::y;
    using typename B2<T>::type;
};
