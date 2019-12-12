// PR c++/85006
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename... Ts> struct A {};

template<typename... Us> A<auto...> foo() { return A{}; }

void bar()
{
  foo();
}
