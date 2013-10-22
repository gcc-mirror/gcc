// { dg-options "-std=c++11" }

#include <initializer_list>

struct A
{
  A& operator=(int i);
  A& operator=(std::initializer_list<int> l) { return *this; }
};

int main()
{
  A a;
  a = { };
}
