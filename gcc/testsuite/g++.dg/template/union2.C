/* PR c++/40557 */
/* { dg-do "compile" } */

struct A
{
  typedef int X;
};

template<int> union B
{
  A::X x;
};
