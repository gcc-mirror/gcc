// PR c++/58760
// { dg-do compile { target c++11 } }

enum en
{
  a,b,c
};
 
struct B
{
  template<en N>
  struct A
  {
    const int X = N;
  };
};
