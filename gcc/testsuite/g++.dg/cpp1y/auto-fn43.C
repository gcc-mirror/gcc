// PR c++/64931
// { dg-do compile { target c++14 } }

template<typename T>
struct S {
  T data[32];
};

auto
foo (S<int> & x)
{
  return x;
}
