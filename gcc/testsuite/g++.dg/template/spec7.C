// { dg-do compile }

// PR c++/6440: Specialization of member class template.

template<class T> struct A
{
  template<class U> struct B {};
}; 

template<> template<class U>
struct A<int>::B
{
  void f();
  template <class V> void g(V);
};

template<> template<> template <class V> void A<int>::B<char>::g(V)
{
}

A<int>::B<char> b;

void h()
{
  b.f();
  b.g(0);
}
