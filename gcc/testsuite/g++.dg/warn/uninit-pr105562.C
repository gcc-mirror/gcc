// { dg-require-effective-target c++11 }
// { dg-options "-O -Wall -fno-strict-aliasing" }
// { dg-skip-if "requires hosted libstdc++ for regex" { ! hostedlib } }

#include <regex>

int main()
{
  std::regex a(".");
  std::regex b(std::move(a));
}
