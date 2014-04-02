// PR c++/42056
// { dg-do compile { target c++11 } }

template<int> struct A
{
  int a[auto(1)]; // { dg-error "invalid use of" }
};

template<int> void foo()
{
  int a[auto(1)]; // { dg-error "invalid use of" }
}
