// PR c++/96097
// { dg-do compile }

template <template <typename T, typename T::type TT> class X>
void func() {}

template <typename U, int I>
struct Y {};

void test()
{
  func<Y>();
}
