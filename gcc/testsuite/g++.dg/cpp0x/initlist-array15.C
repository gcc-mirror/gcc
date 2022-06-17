// PR c++/71424
// { dg-do compile { target c++11 } }

#include <initializer_list>

void f(std::initializer_list<double[2]> list)
{
}

int main()
{
   f({ { 1.0, 2.0 }, { 3.0, 4.0 } });
}
