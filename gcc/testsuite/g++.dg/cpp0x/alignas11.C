// PR c++/80241
// { dg-do compile { target c++11 } }

template <typename... T>
struct A
{
  [[gnu::aligned (alignof(A))...]] char c; // { dg-error "expansion pattern" }
};

A<int> a;
