// PR c++/50276
// { dg-options "-Wuninitialized" }
// { dg-do run { target c++11 } }

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
