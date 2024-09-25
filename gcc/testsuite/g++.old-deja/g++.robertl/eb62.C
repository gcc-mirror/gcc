// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
#include <vector>

void f(void)
{
  std::vector<int> l(5, 0);
}
