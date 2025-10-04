// { dg-do run { target c++11 } }
// { dg-options "-Wno-vla" }

#include <initializer_list>

struct A
{
  int i;
  A(std::initializer_list<int>) : i{43} { }
  A(int i): i{i} { }
  ~A() {}
};

int x = 4;
int main(int argc, char **argv)
{
  { int i[x] = { 42, 42, 42, 42 }; }
  {
    A a[x] = { argc };
    if (a[1].i != 43)
      __builtin_abort ();
  }
}
