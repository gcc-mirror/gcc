// { dg-options -std=c++0x }

constexpr int zero() { return 0; }

void* ptr1 = zero(); // #1
constexpr void* ptr2 = zero(); // #2
