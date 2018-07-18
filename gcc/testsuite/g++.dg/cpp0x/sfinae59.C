// PR c++/81359
// { dg-do compile { target c++11 } }

template<typename _Tp, typename = decltype(_Tp())>
static int test(int);

template<typename>
static void test(...);

template <class T, class = decltype(test<T>(0))>
struct A { };

struct B
{
  struct C {
    int i = 0;
  };
  A<C> a;
};
