// PR 31246
// { dg-do compile }
// { dg-options "-Wunreachable-code -D_GLIBCXX_DEBUG" }
// { dg-xfail-if "lack of weak symbols" { alpha*-dec-osf* } }
#include <vector>

int main()
{
  std::vector<int>::iterator a;
}
