// { dg-do run }
// { dg-options "-std=c++0x" }

// Test user-defined literals.
// Test simple operator declaration and definition.

#include <cstring>
#include <string>
#include <complex>
#include <cassert>

long double operator"" _v(long double);
std::string operator"" _w(const char16_t*, size_t);
unsigned operator"" _w(const char*);

std::complex<double>
operator"" _i(long double y)
{ return std::complex<double>(0.0L, y); }

void
test1()
{
  long double x = operator"" _v(1.2L);
  assert(x == 2.2L);

  std::string s = operator"" _w(u"one", 3);
  assert(s == "boo");

  unsigned u = operator"" _w("Hello, World!");
  assert(u == 13U);

  std::complex<double> i = operator"" _i(2.0);
  assert(i == std::complex<double>(0.0, 2.0));
}

int
main()
{
  test1();
}

long double
operator"" _v(long double x)
{ return x + 1.0L; }

std::string
operator"" _w(const char16_t*, size_t)
{ return std::string("boo"); }

unsigned
operator"" _w(const char* str)
{ return strlen(str); }
