// PR c++/85279
// { dg-do compile { target c++11 } }

template<typename T> struct A
{
  void foo(decltype(T())::Y);	// { dg-error "decltype\\(T\\(\\)\\)::Y" "" { target c++17_down } }
};
