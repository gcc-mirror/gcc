// PR c++/85006
// { dg-additional-options "-std=c++17 -fconcepts" }

template<typename... Ts> struct A {};

template<typename... Us> A<auto...> foo() { return A{}; }

void bar()
{
  foo();
}
