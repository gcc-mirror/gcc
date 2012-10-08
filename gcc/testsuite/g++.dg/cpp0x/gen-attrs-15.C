// { dg-do compile { target c++11 } }
// PR c++/15317

struct A
{
  A(char);
};
A::A([[gnu::unused]] char i2)
{}
