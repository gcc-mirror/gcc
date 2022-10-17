// Test C++17 hardware interference size constants
// { dg-do compile { target c++17 } }

#include <new>

// It is generally agreed that these are the right values for all x86.
static_assert(std::hardware_destructive_interference_size == 64);
static_assert(std::hardware_constructive_interference_size == 64);
