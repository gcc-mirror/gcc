#include <iostream.h>
#include <stddef.h>
#include <new>

int fail = 1;

static void *operator new(size_t size) throw (std::bad_alloc) {
  --fail;
  return (void*) 0;
}

int main() {
  cout << "";
  new int;
  return fail;
}
