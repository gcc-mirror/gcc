// PR c++/13529

#include <cstddef>

struct A { int i; };
struct B { A a; };

int main()
{
  return offsetof(B,a.i) != 0;
}
