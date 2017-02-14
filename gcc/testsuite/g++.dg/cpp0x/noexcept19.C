// PR c++/55652
// { dg-do compile { target c++11 } }

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
  D ()
#if __cplusplus <= 201402L
  throw (int)			// { dg-warning "deprecated" "" { target { ! c++1z } } }
#endif
  ;
};

C <D, B <D>> c;
