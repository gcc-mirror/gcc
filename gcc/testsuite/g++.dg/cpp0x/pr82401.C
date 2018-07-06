// PR c++/82401
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T> struct A
{
  enum E : T;
  void h ();
};
template <typename T> enum A<T>::E : T { e1, e2 };
template <> enum A<long long>::E : long long {};
template <typename T> struct C
{
  enum class E : T;
};
C<int>::E c3 = C<int>::E::e1;	// { dg-error "is not a member of" }
