// PR c++/19966
// { dg-do compile }

struct A
{
  static operator int(); // { dg-error "10:.static A::operator int\\(\\). must be a nonstatic member function" }
};

struct B
{
  static int operator*(); // { dg-error "14:.static int B::operator\\*\\(\\). must be either a non-static member function or a non-member function" }
};

static operator int(); // { dg-error "8:.operator int\\(\\). must be a nonstatic member function" }
