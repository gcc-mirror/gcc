#include <iostream.h>
#include <stddef.h>

int fail = 1;

static void *operator new(size_t size) {
  --fail;
  return (void*) 0;
}

main() {
  cout << "";
  new int;
  return fail;
}
