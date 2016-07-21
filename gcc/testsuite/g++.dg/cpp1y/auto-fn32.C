// { dg-do compile { target c++14 } }

template<class,class> struct same_type;
template<class T> struct same_type<T,T> {};

struct A
{
  static int b;
  int c;

  template <int>
  decltype(auto) f() { return A::c; }

  template <int>
  decltype(auto) g() { return (A::c); }
};

A a;

template <int>
decltype(auto) f() { return A::b; }

template <int>
decltype(auto) g() { return (A::b); }

int main()
{
  same_type<decltype(f<0>()), int>();
  same_type<decltype(g<0>()), int&>();

  same_type<decltype(a.f<0>()), int>();
  same_type<decltype(a.g<0>()), int&>();
}
