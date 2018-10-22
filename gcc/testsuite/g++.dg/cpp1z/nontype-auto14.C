// PR c++/82231
// { dg-do compile { target c++17 } }

template<int>
struct x {};

template <auto I, decltype(I) X>
void f(x<X>) { }

int
main()
{
  f<0>(x<1>{});
}
