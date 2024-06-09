// PR c++/113347

#if __cplusplus < 201103L
#define THROWS
#else
#define THROWS noexcept(false)
#endif

struct A { ~A(); };
struct B { ~B() THROWS; };

A f()
{
  A a;
  return a;
  B();
}
