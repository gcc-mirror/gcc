// Test constexpr virtual in non-primary vtable.
// { dg-do compile { target c++20 } }

struct A
{
  virtual constexpr int f() const { return 1; };
};

struct B
{
  virtual constexpr int g() const { return 2; };
};

struct C: A, B
{
};

constexpr C c;

constexpr int g(const B& b) { return b.g(); }
static_assert (g(c) == 2);

