// { dg-options "-std=c++0x" }

template<class T> int foo(int a)
{
  const unsigned b = a < 0 ? -a : a;
  return 0;
}

int i = foo<float>(1);
