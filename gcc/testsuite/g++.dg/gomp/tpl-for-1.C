// { dg-do compile }

void foo(int);
void foo(long);

template<typename T> void bar()
{
  #pragma omp for
  for (T i = 0; i < 10; ++i)
    foo(i);
}

void test()
{
  bar<int>();
  bar<long>();
}
