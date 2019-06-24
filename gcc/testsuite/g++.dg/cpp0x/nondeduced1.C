// PR c++/60223
// { dg-do compile { target c++11 } }

template<typename T, T = T{}>
struct A { };

template<typename T>
void foo(A<T> a);

void bar()
{
  foo(A<char, char{}>());
  foo(A<char>());
  foo<>(A<char>());
  foo<>(A<char, char{}>());
}
