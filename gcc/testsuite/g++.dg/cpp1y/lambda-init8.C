// DR1760: "no additional copy and destruction is performed"
// { dg-do run { target c++14 } }

#include <cassert>

int copy_count = 0;
int dtor_count = 0;

struct X
{
  X() = default;
  X(const X&) { ++copy_count; }
  ~X() { ++dtor_count; }
};

int main()
{
  {
    X x;
    auto z = [y = x](){};
    X x2;
    auto z2 = [x2](){};
    assert(copy_count == 2);
  }
  assert(dtor_count == 4);
}
