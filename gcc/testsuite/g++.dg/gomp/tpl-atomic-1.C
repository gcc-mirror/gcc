// { dg-do compile }

int check;

template<typename T> void foo()
{
  #pragma omp atomic
  check |= sizeof(T);
}

template<typename T> void bar(T *x, T y)
{
  #pragma omp atomic
  *x += y;
}

void test ()
{
  int i;
  long l;

  foo<char>();
  foo<short>();
  bar(&i, 4);
  bar(&l, 8L);
}
