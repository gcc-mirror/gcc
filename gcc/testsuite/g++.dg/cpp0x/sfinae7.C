// { dg-options -std=c++11 }

struct A
{
  void f();
  void f(int);
  typedef int g;
};

template <class T> decltype (T::f) f();
template <class T> void f();

template <class T> decltype (T::g) g();
template <class T> void g();

int main()
{
  f<A>();
  g<A>();
}
