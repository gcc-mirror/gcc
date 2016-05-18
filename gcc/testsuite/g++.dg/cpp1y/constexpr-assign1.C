// { dg-do compile { target c++14 } }

struct A { };

struct B
{
  A a;
  constexpr B& operator=(const B&) = default;
};
