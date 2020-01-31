/* { dg-do compile } */
/* { dg-options "-O2 -std=c++17 -fdump-tree-cddce-details" } */
/* { dg-additional-options "-fdelete-null-pointer-checks" } */

#include <cstdio>
#include <cstdlib>
#include <new>

void* operator new(std::size_t sz)
{
  std::printf("global op new called, size = %zu\n", sz);
  void *ptr = std::malloc(sz);
  if (ptr)
    return ptr;
  else
    throw std::bad_alloc{};
}

void operator delete(void* ptr) noexcept
{
  std::puts("global op delete called");
  std::free(ptr);
}

void
new_primitive_load() {
  int *x = new int;
  int tmp = *x;
  delete x;
}

void
new_array_load() {
  int *x = new int[10];
  int tmp = x[4];
  delete [] x;
}

/* { dg-final { scan-tree-dump-times "Deleting : _\\d+ = operator new" 2 "cddce1"} } */
/* { dg-final { scan-tree-dump-times "Deleting : operator delete" 2 "cddce1"} } */
