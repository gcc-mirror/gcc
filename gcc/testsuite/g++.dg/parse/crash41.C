// PR c++/35327

struct A
{
  A(int)(); // { dg-error "declared" }
};

template<int> void foo(bool b, A a) { b ? a : 0; } // { dg-error "?:" }
