// PR c++/51666 (DR 325)
// { dg-do compile { target c++11 } }

template<typename T, typename U>
struct tuple
{
  tuple(T, U) { }

  static const int x = 3;
  int var = tuple<T,U>::x;
};

struct Y
{
  tuple<int, int> tt = tuple<int, int>{1, 2};
};

struct A
{
  int i = 0;
  int j = i < 42, k;		// OK, declares j and k
  int l = i < 42, 24;		// { dg-error "" }
};
