// PR c++/105839
// { dg-do compile { target c++14 } }
// { dg-options "-fopenmp" }

template <typename T>
void
foo (const T &x)
{
  [&] (auto &&y)
  {
    #pragma omp parallel for
    for (auto &&[v1, v2] : x)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
      ;
  } ([]{});
}

struct A { int a, b; };

void
bar ()
{
  A a[10];
  foo (a);
}
