// PR c++/118355
// { dg-do compile { target c++11 } }

enum MY_ENUM
{
  ZERO,
};

struct FOO
{
  MY_ENUM type = ZERO;
};

struct ARR
{
  FOO array[1] = {};
};

template<typename>
struct ARR2
{
  FOO array[1] = {};
};

void
g ()
{

  ARR arr;
  arr = {};

  ARR2<int> arr2;
  arr2 = {};
}
