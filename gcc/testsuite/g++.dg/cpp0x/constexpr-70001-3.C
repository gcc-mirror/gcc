// PR c++/70001
// { dg-do compile { target c++11 } }

#include <array>
#include <complex>

typedef std::complex<double> cd;

const int LOG = 17;
const int N = (1 << LOG);

std::array<cd, N> a;
std::array<cd, N> b;

void
foo (std::array<cd, N> &arr)
{
  std::array<std::array<cd, N>, LOG + 1> f;
}

int
main ()
{
  foo (a);
  foo (b);
}
