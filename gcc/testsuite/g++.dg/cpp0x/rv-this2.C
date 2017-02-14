// PR c++/56701
// { dg-do compile { target c++11 } }

struct A
{
  void f(){ A*&& a = &*this; }
};
int main(){}
