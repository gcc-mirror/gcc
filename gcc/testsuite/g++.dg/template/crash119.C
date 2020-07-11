// PR c++/59115

template<typename T, float, int, typename U> void foo(T, U) {} // { dg-error "valid type" "" { target c++17_down } }

void bar()
{
  foo(0, 0);  // { dg-error "matching" }
}
