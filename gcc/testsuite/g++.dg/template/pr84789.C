// { dg-do compile }

struct A
{
  typedef int I;
};

template<typename> struct B : A {};

template<typename T> struct C : B<T>
{
  B<T>::A::I::I i; // { dg-error "not a class type|does not name a type|typename" "" { target c++17_down } }
};
