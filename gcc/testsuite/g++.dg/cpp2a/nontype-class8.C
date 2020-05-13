// If the entity is a template parameter object for a template parameter of
// type T, the type of the expression is const T.

// { dg-do compile { target c++20 } }

template <class T, class U> struct same;
template <class T> struct same<T,T> {};

struct A {
  int i;
  // auto operator<=> (const A&) = default;
};
void f(A&) = delete;
void f(const A&) { }

template < A a > struct B
{
  B()
  {
    f(a);
    same<A,decltype(a)> s;
    same<const A&,decltype((a))> s2;
  }
};

B<A{42}> b;
