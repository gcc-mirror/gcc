// { dg-options -std=c++0x }

constexpr unsigned long long
operator"" _grow(unsigned long long n)
{ return 2 * n; }

double buffer[25_grow];
