// PR c++/49216
// { dg-options -std=c++0x }
// { dg-do run }

#include <initializer_list>
extern "C" void abort();

bool constructed;

struct A
{
  A(std::initializer_list<int>) { constructed = true; }
};

int main() {
  new A[1]{};
  int *p = new int[1]{};
  if (p[0] != 0 || !constructed)
    abort();
}
