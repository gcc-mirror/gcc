// Test C++17 hardware interference size constants
// { dg-do compile { target c++17 } }

#include <new>

// Recent ARM CPUs have a cache line size of 64.  Older ones have
// a size of 32, but I guess they're old enough that we don't care?
static_assert(std::hardware_destructive_interference_size == 64);
static_assert(std::hardware_constructive_interference_size == 64);
