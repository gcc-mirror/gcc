// DR 2392
// { dg-do compile { target c++11 } }

template <class T = void>
constexpr int
foo ()
{
  T t;
  return 1;
}

using V = decltype (new int[foo ()]);
