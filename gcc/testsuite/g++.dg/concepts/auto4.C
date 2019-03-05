// PR c++/85006
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template<typename... Ts> struct A {};

template<typename... Us> A<auto...> foo() { return A{}; }

void bar()
{
  foo();
}
