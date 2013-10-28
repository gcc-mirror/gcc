// Test for initlist lifetime
// { dg-options "-std=c++11" }
// { dg-do run }

#include <initializer_list>

int c;

struct A
{
  A(int,int) { ++c; }
  ~A() { --c; }
};

void f (std::initializer_list<A> l) { }

int main()
{
  f({ {1,2}, {3,4} });
  if (c != 0)
    return 1;

  {
    std::initializer_list<A> l { {1,2}, {3,4} };
    if (c != 2)
      return 2;
  }
  if (c != 0)
    return 3;
}
