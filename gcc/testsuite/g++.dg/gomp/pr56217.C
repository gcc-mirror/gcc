// PR middle-end/56217
// { dg-do compile { target c++20_down } }
// { dg-options "-fopenmp" }

struct S { int *p; S (); S (S &); };

S
foo ()
{
  S s;
  #pragma omp task shared (s)
    s.p = 0;
  // This fails in C++23, because "cannot bind non-const lvalue reference of
  // type 'S&' to an rvalue of type 'S'".
  return s;
}
