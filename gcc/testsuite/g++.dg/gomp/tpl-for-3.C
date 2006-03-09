// { dg-do compile }

void foo(int);

template<typename T> void bar()
{
  #pragma omp parallel for
  for (typename T::T i = 0; i < T::N; ++i)
    foo(i);
}

struct A
{
  typedef int T;
  static T N;
};

struct B
{
  typedef long T;
  static T N;
};

void test()
{
  bar<A>();
  bar<B>();
}
