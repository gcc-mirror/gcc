// PR c++/42056
// { dg-do compile { target c++11 } }

template<int> struct A
{
  int a[auto(1)]; // { dg-error "9:only available" "" { target c++20_down } }
};

template<int> void foo()
{
  int a[auto(1)]; // { dg-error "9:only available" "" { target c++20_down } }
}
