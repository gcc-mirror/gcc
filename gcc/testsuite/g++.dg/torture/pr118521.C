// { dg-do compile }
// { dg-additional-options "-Wall" }

#include <vector> // { dg-bogus "writing 1 byte into a region of size 0" "" { target *-*-* } 0 }

void bar(std::vector<char>);

void foo()
{
  std::vector<char> v{1, 2};
  v.insert(v.end(), 2, 0);
  v.push_back(1);
  bar(v);
}
