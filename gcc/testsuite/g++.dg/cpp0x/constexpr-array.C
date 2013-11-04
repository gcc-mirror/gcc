// { dg-options -std=c++11 }
// { dg-final { scan-assembler-not "static_initialization" } }

struct A
{
  int i;
  constexpr A(): i(0) { }
};

struct B
{
  A a[4];
};

extern const B b{};
