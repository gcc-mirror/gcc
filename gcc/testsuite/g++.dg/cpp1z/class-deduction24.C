// PR c++/78894
// { dg-options -std=c++17 }

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
