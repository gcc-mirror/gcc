// PR c++/51225
// { dg-options "-std=c++11" }

template<int> struct A {};

template<typename> void foo()
{
  A<int(x)> a; // { dg-error "not declared|invalid type" }
}

template<typename> struct bar
{
  static constexpr A<1> b = A<1>(x); // { dg-error "not declared" }
};
