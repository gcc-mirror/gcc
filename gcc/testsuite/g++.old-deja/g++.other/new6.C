// { dg-do run  }
// Test that we properly default-initialize the new int when () is given.

#include <new>
using namespace std;
extern "C" void *malloc (size_t);

typedef int int32_t __attribute__((mode (__SI__)));

int32_t special;
int32_t space = 0xdeadbeef;

void *operator new (size_t size)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{
  if (special)
    return &space;
  return malloc (size);
}

int main ()
{
  special = 1;
  int32_t *p = new int32_t();
  special = 0;
  return *p != 0;
}
