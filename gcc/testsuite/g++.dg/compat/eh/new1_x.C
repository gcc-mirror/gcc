#include <new>
#include <cstddef>

extern "C" void exit (int);
extern "C" void abort (void);

extern void * operator new[] (std::size_t s) throw (std::bad_alloc);
extern void operator delete[] (void *p) throw ();

struct A
{
  A() { throw 1; }
  ~A() {}
};

int ret = 1;

void
new1_x ()
{
  try
    {
      A *p = new A[4];
    }
  catch (...) {}
  if (ret != 0)
    abort ();
  exit (0);
}
