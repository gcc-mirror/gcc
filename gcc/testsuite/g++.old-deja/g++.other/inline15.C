// { dg-do assemble { target c++17_down } }
// { dg-options "-O1" }
// Origin: Jakub Jelinek <jakub@redhat.com>

class Type;
template<class E>
class X
{
public:
  X<E>();
  inline X<E>(int);
  inline ~X<E>();
};
template<class E> const Type &foo(const X<E> *);
template<class E> inline X<E>::X(int x)
{
  const Type &a = foo(this);
}
template<class E> inline X<E>::~X()
{
  const Type &a = foo(this);
}
class Y
{
  X<Type> a;
public:
  Y(const X<Type> &x = X<Type>());
};
Y::Y(const X<Type> &x) : a(1)
{
}
