// PR c++/51225
// { dg-do compile { target c++11 } }

template<int> struct A {};

template<typename> void foo()
{
  A<int(x)> a; // { dg-error "not declared|could not convert" }
}

template<typename> struct bar
{
  static constexpr A<1> b = A<1>(x); // { dg-error "not declared" }
};
