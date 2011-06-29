// PR c++/35240
// { dg-do compile }


template<int> struct A {};

template<int N> A<sizeof(new int[N][N])> foo(); // { dg-message "unimplemented" }

void bar()
{
  foo<1>(); // { dg-message "required" }
}
