// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Wall" }

#include <cstdlib>
#include <vector>

typedef std::vector<int> v1;

static
void drop_inplace(v1 & c, size_t len)
{
  if (len <= c.size())
    c[len-1] = 0;  /* { dg-bogus "outside array bounds" } */
}

void func()
{
  v1 vec1{1,2,3,4,5,6};
  drop_inplace(vec1, 10);
}
