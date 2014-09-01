// { dg-do compile { target c++14 } }

struct X
{
  template <class T> static int x;
};

int X::x = 42;			// { dg-error "template" }

struct Y
{
  static int y;
};

template <class T> int Y::y = 42; // { dg-error "template" }
