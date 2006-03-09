// { dg-do compile }

void foo(int);

template<int A, int B, int C> void bar()
{
  #pragma omp for
  for (int i = A; i < B; i += C)
    foo(i);
}

void test()
{
  bar<0, 10, 2>();
}
