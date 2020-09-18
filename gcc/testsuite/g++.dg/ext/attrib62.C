// PR c++/35098
// { dg-do compile }

template<typename T> struct A
{
  T a, __attribute((unused)) b; // { dg-warning "attribute ignored" }
};
