// { dg-additional-options "-Wno-c++11-extensions" }
// PR c++/15317

struct A
{
  A(char);
};
A::A([[gnu::unused]] char i2)
{}
