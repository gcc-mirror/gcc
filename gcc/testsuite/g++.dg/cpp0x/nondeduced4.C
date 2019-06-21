// PR c++/60223
// { dg-do compile { target c++11 } }

template<typename T>
struct A { };

template<typename T>
void foo(A<T>, T = T{});

void bar()
{
  foo(A<int>());
}
