/* { dg-do compile { target c++11 } } */

/* Dependent function calls in a clause.  */

/* Not constexpr, invalid value.  */
template<typename T>
int get_align(T) { return 42; }

template<typename T>
void f()
{
  int a = 42;
  #pragma omp allocate(a) align(get_align(T{}))
}

namespace foo
{
  struct S {};

  constexpr int get_align(S) { return 32; }
}

void instantiate()
{
  f<foo::S>();
}
