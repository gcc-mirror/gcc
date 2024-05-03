// { dg-do run { target c++11 } }

#include <initializer_list>

int as;
struct A {
  A(const char *) { ++as; }
  A(const A&) { ++as; }
  ~A() { --as; }
};

void __attribute__((noipa))
tata(std::initializer_list<A> init)
{
  throw 1;
}

int
main()
{
  try { tata({ "foo","bar" }); }
  catch (...) { }

  if (as != 0) __builtin_abort ();
}
