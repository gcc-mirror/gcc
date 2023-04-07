// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2" }

#include <vector>

void test01()
{
  std::vector<int> v1, v2{5, 6};
  int n = 0;
  std::vector<int>::iterator it = v1.insert(v1.cbegin(), n);
  it = v1.insert(v1.cbegin(), 1);
  it = v1.insert(v1.cbegin(), {2, 3});
  it = v1.insert(v1.cbegin(), 1, 4);
  it = v1.insert(v1.cbegin(), v2.begin(), v2.end());
}
