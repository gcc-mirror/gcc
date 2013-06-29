
#include <complex>

#pragma GCC system_header

std::complex<float>
operator""if(long double ximag)
{ return std::complex<float>(0.0F, static_cast<float>(ximag)); }

std::complex<float>
operator""if(unsigned long long nimag)
{ return std::complex<float>(0.0F, static_cast<float>(nimag)); }
