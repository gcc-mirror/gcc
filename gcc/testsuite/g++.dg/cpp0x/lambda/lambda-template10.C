// PR c++/50276
// { dg-options "-std=c++11 -Wuninitialized" }
// { dg-do run }

template<typename T>
unsigned testfun(const T& func)
{
  return func();
}

template<int i>
unsigned test()
{
  if (unsigned value = testfun( [] () { return 0; }))
    return value;
  return i;
}

int main()
{
  if (test<42>() != 42)
    __builtin_abort ();
}
