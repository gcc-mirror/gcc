// PR c++/57820
// { dg-do compile { target c++11 } }

struct C
{
  int a = 2;
  int b = a + 1;
};

C c;
constexpr C d = {};
