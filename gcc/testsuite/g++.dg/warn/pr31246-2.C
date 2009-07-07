// PR 31246
// { dg-do compile }
// { dg-options "-Wunreachable-code" }
#include <new>

int* get_ptr(void* ptr)
{
  return new(ptr) int();
}
