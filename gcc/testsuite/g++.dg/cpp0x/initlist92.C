// PR c++/64665, DR 1467 
// { dg-do run { target c++11 } }

#include <string>
#include <cassert>

bool Test1(bool) 
{
  return true;
}
bool Test1(std::string)
{
  return false;
}

bool Test2(int)
{
  return false;
}
bool Test2(std::initializer_list<int>)
{
  return true;
}

struct S 
{ 
    S(int _a) : a(_a) {}
private:
    int a;
};
bool Test3(int)
{
  return true;
}
bool Test3(S)
{
  return false;
}

bool Test4(bool) 
{
  return false;
}
bool Test4(std::initializer_list<std::string>)
{
  return true;
}

int main () 
{
  assert ( Test1({"false"}) );
  assert ( Test2({123}) );
  assert ( Test3({456}) );
  assert ( Test4({"false"}) );
}
