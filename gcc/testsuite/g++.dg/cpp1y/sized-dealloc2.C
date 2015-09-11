// Test for a diagnostic about a usual deallocation function used as a
// placement deallocation function.  This will be a warning in C++98/11
// modes and an error in C++14 mode.

// { dg-options "-Wc++14-compat" }

#include <new>
void *operator new (std::size_t s, std::size_t)
{
  return operator new (s);
}

void operator delete (void *p, std::size_t) throw()
{
  return ::operator delete (p);
}

struct A
{
  A();
};

void f()
{
  new (42) A;		// { dg-message "" }
}
