// PR c++/87109
// { dg-do run { target c++11 } }

#include <utility>

struct Y {
  int y;
  Y(int y_) : y(y_) { }
};
struct X : public Y {
  int x;
  X(int x_, int y_) : x(x_), Y(y_) { }
};

struct A {
  operator X() & { return { 0, 2 }; }
  operator X() && { return { 0, -1 }; }
};

Y
f (A a)
{
  return a;
}

Y
f2 (A a)
{
  return std::move (a);
}

Y
f3 ()
{
  A a;
  return a;
}

Y
f4 ()
{
  A a;
  return std::move (a);
}

Y
f5 ()
{
  return A();
}

int
main ()
{
  Y y1 = f (A());
  if (y1.y != 2)
    __builtin_abort ();
  Y y2 = f2 (A());
  if (y2.y != -1)
    __builtin_abort ();
  Y y3 = f3 ();
  if (y3.y != 2)
    __builtin_abort ();
  Y y4 = f4 ();
  if (y4.y != -1)
    __builtin_abort ();
  Y y5 = f5 ();
  if (y5.y != -1)
    __builtin_abort ();
}
