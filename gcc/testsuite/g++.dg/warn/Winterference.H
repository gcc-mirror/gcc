#include <new>

struct A
{
  alignas(std::hardware_destructive_interference_size) int i;
  alignas(std::hardware_destructive_interference_size) int j;
};
