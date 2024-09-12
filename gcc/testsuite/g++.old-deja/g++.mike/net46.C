// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
#include <iostream>
#include <cstdlib>
#include <cstddef>
#include <new>

int fail = 1;

int in_main = 0;

void *operator new(size_t size)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{
  if (!in_main) return malloc (size);
  --fail;
  return (void*) 0;
}

int main() {
  std::cout << "";
  in_main = 1;
  new int;
  return fail;
}
