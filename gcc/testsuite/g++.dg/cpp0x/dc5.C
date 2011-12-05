// { dg-do run }
// { dg-options "--std=c++0x" }

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
