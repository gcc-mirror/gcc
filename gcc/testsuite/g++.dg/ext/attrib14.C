// PR c++/15317

struct A
{
  A(char);
};
A::A(__attribute__((unused)) char i2)
{}

