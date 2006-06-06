// PR c++/27177

struct X {};

struct Y : virtual X {};
struct Z : virtual X {};

struct A : Y, Z {};

struct B : A
{
  static const int i = sizeof((Z*)(B*)0);
};
