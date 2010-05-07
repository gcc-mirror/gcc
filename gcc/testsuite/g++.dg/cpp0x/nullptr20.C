// { dg-do run }
// { dg-options "-std=c++0x" }

// Test passing to ellipisis

#include <cstdio>
#include <cstring>

int main()
{
  char buf1[64];
  char buf2[64];

  std::sprintf(buf1, "%p", (void*)0);
  std::sprintf(buf2, "%p", nullptr);
  return std::strcmp(buf1, buf2) != 0;
}
