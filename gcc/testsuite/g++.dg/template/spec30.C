// { dg-do compile }

template<int N>
inline int
foo (int a)
{
  return a;
}

template<>
inline int
foo<0> (int a = 123) // { dg-error "default argument" }
{
  return a + 1;
}
