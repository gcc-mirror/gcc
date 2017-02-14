// PR c++/78894
// { dg-options -std=c++1z }

struct A
{
  A ();
};
template <typename T>
struct C
{
  C (int, const T &, const A & = A ());
};

C a = { 0, 0 };
