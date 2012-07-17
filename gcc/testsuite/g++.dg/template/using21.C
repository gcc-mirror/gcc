// PR c++/52126
// { dg-do compile }

template<typename T>
struct A
{
  int foo;

  struct B;
  struct C;
  struct D;
  struct E;
};

template <class T>
struct A<T>::B : A<T>
{
  using A::foo;
};

template <class T>
struct A<T>::C : A
{
  using A::foo;
};

template <class T>
struct A<T>::D : A<T>
{
  using A<T>::foo;
};

template <class T>
struct A<T>::E : A
{
  using A<T>::foo;
};
