// PR c++/31434
// { dg-do run }
// { dg-options "-std=c++0x" }

extern "C" void abort ();

template<typename... T> inline int foo (const T...) { return 1; }
template<typename... T> inline int foo (const T *...) { return 2; }

void
bar (int *a)
{
  a[0] = foo (0);
  a[1] = foo (*a);
  a[2] = foo<int> (a);
  a[3] = foo<int> (2, 3, 4, 5);
  a[4] = foo<int> (a, a + 1, a + 2);
}

int
main ()
{
  int a[5];
  bar (a);
  if (a[0] != 1 || a[1] != 1 || a[2] != 2 || a[3] != 1 || a[4] != 2)
    abort ();
  return 0;
}
