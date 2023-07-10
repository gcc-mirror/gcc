// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O3 -fno-exceptions -Wdangling-pointer" }

#include <cstdint>
#include <vector>

struct Data {
  std::vector<uint16_t> v = {1, 1};
};

int main()
{
  Data a;
  Data b;
}
