// PR c++/52126
// { dg-do compile }

template <class T> struct Z {};

template<typename T>
struct A
{
  struct B;
  struct C;
  struct D;
  struct E;
  struct F;
};

template <class T>
struct A<T>::B : A<T>
{
  using A::nonexist; // { dg-error "has not been declared" }
};

template <class T>
struct A<T>::C : A
{
  using A::nonexist; // { dg-error "has not been declared" }
};

template <class T>
struct A<T>::D : A<T>
{
  using A<T>::nonexist; // { dg-error "has not been declared" }
};

template <class T>
struct A<T>::E : A
{
  using A<T>::nonexist; // { dg-error "has not been declared" }
};

template <class T>
struct A<T>::F : Z<T>
{
  using Z<T>::nonexist;
};
