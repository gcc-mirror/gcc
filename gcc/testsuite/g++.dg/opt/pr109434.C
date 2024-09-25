// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-options "-O2 -Wall" }
// { dg-skip-if "requires hosted libstdc++ for stdexcept" { ! hostedlib } }

#include <optional>
#include <stdexcept>

std::optional<int> foo()
{
  volatile int x = 1;
  if (x)
    throw std::runtime_error("haha");
  return 42;
}

int main()
{
  std::optional<int> optInt;
  try {
      // We falsely DSEd the LHS of the call even though foo throws
      // which results in an uninitialized diagnostic
      optInt = foo();
  } catch (...) {
      return optInt.has_value();
  }
  std::optional<double> optDbl{optInt};
  return optDbl ? optDbl.value () : 2.0;
}
