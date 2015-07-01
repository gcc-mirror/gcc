// PR c++/66686

template <int>
struct Y { };

template <class B, template <template <B> class Z> class C>
struct X
{
  C<Y> a;  // { dg-bogus "mismatch" }
};

template <template <int> class>
struct A { };

X<int, A> a;
