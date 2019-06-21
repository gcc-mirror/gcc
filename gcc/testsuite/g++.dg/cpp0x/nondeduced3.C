// PR c++/60223
// { dg-do compile { target c++11 } }

template<typename T, T = T{1}>
struct A { };

template<typename T>
void foo(A<T> a);

void bar()
{
  foo(A<char>());
  foo(A<char, char{1}>());
  foo<>(A<char>());
  foo<>(A<char, char{1}>());
}
