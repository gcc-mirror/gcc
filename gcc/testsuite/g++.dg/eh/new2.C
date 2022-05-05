// PR c++/104007
// { dg-do run }

extern "C" void abort();
#include <new>

struct S { ~S() { abort(); } };
int main() {
  new (std::nothrow) S[1];
}
