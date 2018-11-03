// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<class>
struct X {
  int first = 0;
};

int
f ()
{
  X<int> x, y;
  bool b = x.first < y.first;
  return b;
}
