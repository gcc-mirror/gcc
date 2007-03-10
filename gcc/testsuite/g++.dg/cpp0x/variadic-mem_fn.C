// { dg-options "-std=gnu++0x" }
// { dg-do "run" }
// A basic implementation of TR1's mem_fn using variadic teplates
// Contributed by Douglas Gregor <doug.gregor@gmail.com>
#include <cassert>

template<typename R, typename Class, typename... Args>
class Mem_fn
{
 public:
  explicit Mem_fn(R (Class::*pmf)(Args...)) : pmf(pmf) { }

  R operator()(Class& object, Args... args)
  {
    return (object.*pmf)(args...);
  }

  R operator()(Class* object, Args... args)
  {
    return (object->*pmf)(args...);
  }

  R (Class::*pmf)(Args...);
};

template<typename R, typename Class, typename... Args>  
inline Mem_fn<R, Class, Args...>
mem_fn(R (Class::* pmf)(Args...))
{
  return Mem_fn<R, Class, Args...>(pmf);
}

class X {
 public:
  int negate(int x) { return -x; }
  int plus(int x, int y) { return x + y; }
};

int main()
{
  X x;
  X* xp = &x;

  assert(mem_fn(&X::negate)(x, 17) == -17);
  assert(mem_fn(&X::negate)(xp, 17) == -17);
  assert(mem_fn(&X::plus)(x, 17, 25) == 42);
  assert(mem_fn(&X::plus)(xp, 17, 25) == 42);

  return 0;
}
