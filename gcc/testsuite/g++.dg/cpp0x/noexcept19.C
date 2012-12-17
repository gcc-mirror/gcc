// PR c++/55652
// { dg-do compile }
// { dg-options "-std=c++11" }

template <typename T>
struct A
{
  static const bool a = false;
};

template <typename X, typename Y = A <X>>
struct B
{
  B () noexcept (A <Y>::a) {}
};

template <typename X, typename Y>
struct C
{
  X x;
  Y y;
};

struct D
{
  D () throw (int);
};

C <D, B <D>> c;
