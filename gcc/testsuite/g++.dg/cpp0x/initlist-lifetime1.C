// Test that we properly extend the lifetime of the initializer_list
// array even if the initializer_list is a subobject.
// { dg-options -std=c++11 }
// { dg-do run }

#include <initializer_list>

extern "C" void abort();
bool ok;

bool do_throw;

struct A {
  A(int) { if (do_throw) throw 42; }
  ~A() { if (!ok) abort(); }
};

typedef std::initializer_list<A> AL;
typedef std::initializer_list<AL> AL2;
typedef std::initializer_list<AL2> AL3;

struct B {
  AL al;
  const AL& alr;
};

int main(int argc, const char** argv)
{
  do_throw = (argc > 1);	// always false, but optimizer can't tell
  AL ar[] = {{1,2},{3,4}};
  B b = {{5,6},{7,8}};
  AL3 al3 = {{{1},{2},{3}}};
  ok = true;
}
