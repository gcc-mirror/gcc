// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

int count = 0;
struct VB
{
  VB() {++count;}
};

struct B : virtual VB
{
  B() : B(42) {}
  B(int)  {}
};

struct D : B
{
  D() {}
  D(int) : D() {}
};

int main()
{
  D d{42};
  assert(count == 1);
}
