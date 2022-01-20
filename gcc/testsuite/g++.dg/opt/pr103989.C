// { dg-require-effective-target c++17 }
// { dg-options "-Og -Wall" }

#include <optional>
#include <memory>

struct A {
  A (int a) : a {a} 
  {}

  const std::shared_ptr <int> x;
  int a;
};

class B
{
public:
  B (const std::optional <A>& a)
    : a {a}
  {
  }
public:
  const std::optional <A> a;
};

int
main ()
{
  B b {std::nullopt};
}
