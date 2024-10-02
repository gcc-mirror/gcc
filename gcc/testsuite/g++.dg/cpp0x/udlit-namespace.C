// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cmath" { ! hostedlib } }

// Test user-defined literals.
// Test simple operator declaration and definition in namespaces.

#include <cmath>
#include <limits>

namespace Long
{
  long double operator ""_LL(long double);
}

namespace Short
{
  short
  operator ""_SS(long double x)
  { return std::fmod(x, static_cast<long double>(std::numeric_limits<short>::max())); }
}

void
test1()
{
  long double x = Long::operator ""_LL(1.2L);

  using namespace Short;
  short s = operator ""_SS(1.2L);
  short s2 = 1.2_SS;
}

int
main()
{
  test1();
}

namespace Long
{
  long double
  operator ""_LL(long double x)
  { return x + 2.0L; }
}
