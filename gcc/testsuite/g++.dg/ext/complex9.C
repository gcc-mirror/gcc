// PR c++/28501

struct A
{
  operator int();
};

int i = __real__ A();
