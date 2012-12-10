// PR c++/54170
// { dg-do run { target c++11 } }

#include <cassert> 

struct A;
typedef A* ptr;
typedef int (A::*pmf) (int);
typedef int (A::*pdm);

int total;

void add(int n)
{
  total += n;
}

template <typename RType, typename Callable>
RType Call(Callable native_func, int arg)
{
  return native_func(arg);
}

template <typename RType>
RType do_test(int delta)
{
  return Call<RType>([=](int delta) { add(delta); return nullptr; }, delta);
}

template <typename RType>
void test()
{
  total = 0;
  assert (!do_test<RType>(5));
  assert (total == 5);
  assert (!do_test<RType>(20));
  assert (total == 25);
  assert (!do_test<RType>(-256));
  assert (total == -231);
}

int main()
{
  test<ptr>();
  test<pdm>();
  test<pmf>();
}
