// { dg-do run  }
// Test whether N::operator new is different from ::operator new
#include <new>
#include <cstdlib>

bool success;

namespace N{
  void* operator new(size_t n){
    success = true;
    return std::malloc(n);
  }
}

void *operator new(size_t n)throw(std::bad_alloc)
{
  static bool entered = false;
  if(entered)
    throw std::bad_alloc();
  entered = true;
  void *result = N::operator new(n);
  entered = false;
  return result;
}

int main()
{
  try{
    new int;
  }catch(...){
    return 1;
  }
  return success?0:1;
}
