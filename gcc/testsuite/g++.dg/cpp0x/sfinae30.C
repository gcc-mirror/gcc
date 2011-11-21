// { dg-do compile { target c++11 } }

template <class... T> struct tuple;
template <class T> struct tuple<T> { T t; };

template <class T, class U> struct pair;
template<> struct pair<int,double> { };

template <class... Ts>
struct A
{
  template <class... Us,
            class V = tuple<pair<Ts,Us>...> >
  static void f(Us...)
  {
    V v;
  }
  template <class U>
  static void f(bool);
};

int main()
{
  A<int,float>::f<double>(1.0);
}
