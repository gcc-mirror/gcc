// PR c++/64665, DR 1467 
// { dg-do run { target c++11 } }

#include <string>
#include <cassert>

bool Test1(const bool arg) 
{
  return true;
}
bool Test1(const std::string arg)
{
  return false;
}

bool Test2(const int arg) 
{
  return false;
}
bool Test2(const std::initializer_list<int> arg)
{
  return true;
}

struct S 
{ 
    S(int _a) : a(_a){}
    int getA() const { return a; }
private:
    int a;
};
bool Test3(const int arg) 
{
  return true;
}
bool Test3(const S arg)
{
  return false;
}

bool Test4(const bool arg) 
{
  return false;
}
bool Test4(const std::initializer_list<std::string> arg)
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
