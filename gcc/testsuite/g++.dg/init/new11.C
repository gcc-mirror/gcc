// PR c++/17670
// { dg-do run }

#include <cstdlib>
#include <new>

bool abort_new;
void *operator new[](size_t bytes) throw (std::bad_alloc) { 
  if (abort_new)
    abort(); 
  return operator new (bytes);
}


struct X {};  
int main () {
  // Do not abort until main is running in case startup code uses
  // operator new[].
  abort_new = true;
  new (X);
}
