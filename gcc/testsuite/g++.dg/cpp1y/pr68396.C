// { dg-do compile { target c++14 } }

template <unsigned>
auto f () {
  return 2;
}

template <class>
class A {};

template <int... Ix>
auto g () {
  A<decltype(f<Ix> ())...>();
  return f<2> ();
}
