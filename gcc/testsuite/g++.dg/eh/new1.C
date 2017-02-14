// PR c++/5757
// Test that when a constructor throws in a new-expression, we pass the
// right pointer to operator delete.

// { dg-do run }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }

#include <new>

int ret = 1;

void *ptr;
void * operator new[] (std::size_t s)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{
  ptr = operator new (s);
  return ptr;
}

void operator delete[] (void *p) throw ()
{
  if (p == ptr)
    ret = 0;
  operator delete (p);
}

struct A
{
  A() { throw 1; }
  ~A() {}
};

int
main ()
{
  try
    {
      A *p = new A[4];
    }
  catch (...) {}
  return ret;
}
