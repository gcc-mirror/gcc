// PR c++/85148
// { dg-do compile { target c++14 } }

template<typename T> struct A
{
  T x[1]{(__PTRDIFF_TYPE__)this};
};

void foo()
{
  A<A<__PTRDIFF_TYPE__>> a{};
}
