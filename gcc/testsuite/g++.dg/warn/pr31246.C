// PR 31246
// { dg-do compile }
// { dg-options "-Wunreachable-code -D_GLIBCXX_DEBUG" }
#include <vector>

int main()
{
  std::vector<int>::iterator a;
}
