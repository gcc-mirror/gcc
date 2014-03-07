// PR c++/51369
// { dg-do compile { target c++11 } }

constexpr int x[2][2] = {};

template<int>
void
foo ()
{
  x[0][0];
}
