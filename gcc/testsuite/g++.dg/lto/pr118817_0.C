// { dg-lto-do link }
// { dg-lto-options { { -O3 -fPIC -flto -shared -std=c++20 -Wall } } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }

#include <memory>
#include <vector>
#include <string>

int func()
{
  auto strVec = std::make_unique<std::vector<std::string>>();
  strVec->emplace_back("One");
  strVec->emplace_back("Two");
  strVec->emplace_back("Three");
  return 0;
}
