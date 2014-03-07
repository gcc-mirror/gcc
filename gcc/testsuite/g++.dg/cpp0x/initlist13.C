// PR c++/39056
// { dg-do compile { target c++11 } }

#include <complex>

__complex__ int i {0};
std::complex<int> i2 {0};
