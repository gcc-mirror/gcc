// { dg-require-effective-target c++11 }
// { dg-options "-O -Wall -fno-strict-aliasing" }

#include <regex>

int main()
{
  std::regex a(".");
  std::regex b(std::move(a));
}
