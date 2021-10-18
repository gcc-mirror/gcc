// Test C++17 hardware interference size constants
// { dg-do compile { target c++17 } }

#include <new>

// Most AArch64 CPUs have an L1 cache line size of 64, but some recent ones use
// 128 or even 256.
static_assert(std::hardware_destructive_interference_size == 256);
static_assert(std::hardware_constructive_interference_size == 64);
