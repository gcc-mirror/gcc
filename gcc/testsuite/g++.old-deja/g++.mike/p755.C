// { dg-do run  }
// It checks to see if you can define your own global new operator.
// prms-id: 755

#include <new>

extern "C" void _exit(int);

void* operator new(std::size_t sz)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{
  void* p = 0;
  _exit(0);
  return p;
}

int main () {
  int* i = new int;
  delete i;
  return 1;
}
