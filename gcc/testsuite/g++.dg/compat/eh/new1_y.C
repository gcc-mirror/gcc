#include <new>
#include <cstddef>

extern int ret;

void *ptr;
void * operator new[] (std::size_t s)
#if __cplusplus < 201103L
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

