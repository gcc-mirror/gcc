// PR c++/49216
// { dg-options -std=c++11 }
// { dg-do run }

#include <initializer_list>
extern "C" void abort();
void * operator new[] (__SIZE_TYPE__, void *p) { return p; }

bool constructed;

struct A
{
  A(std::initializer_list<int>) { constructed = true; }
};

int main() {
  new A[1]{};
  int space[1] = { 42 };
  int *p = new (space) int[1]{};
  if (p[0] != 0 || !constructed)
    abort();
}
