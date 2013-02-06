// PR middle-end/56217
// { dg-do compile }
// { dg-options "-fopenmp" }

struct S { int *p; S (); S (S &); };

S
foo ()
{
  S s;
  #pragma omp task shared (s)
    s.p = 0;
  return s;
}
