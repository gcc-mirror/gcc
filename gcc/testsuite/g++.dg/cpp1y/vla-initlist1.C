// { dg-do run }
// { dg-options "-std=c++1y" }

#include <initializer_list>

struct A
{
  int i;
  A(std::initializer_list<int>) { }
  A(int i): i{i} { }
  ~A() {}
};

int x = 4;
int main(int argc, char **argv)
{
  { int i[x] = { 42, 42, 42, 42 }; }
  {
    A a[x] = { argc };
    if (a[1].i != 42)
      __builtin_abort ();
  }
}
