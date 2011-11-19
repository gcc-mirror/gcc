// { dg-do run  }
// Test that we properly default-initialize the new int when () is given.

#include <new>
using namespace std;
extern "C" void *malloc (size_t);

int special;
int space = 0xdeadbeef;

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
  int *p = new int();
  special = 0;
  return *p != 0;
}
