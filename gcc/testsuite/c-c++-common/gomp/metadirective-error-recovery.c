/* { dg-do compile } */

/* This test used to ICE in C and only diagnose the first error in C++.  */

struct s {
  int a, b;
};

void f (int aa, int bb)
{
  struct s s1, s2;
  s1.a = aa;
  s1.b = bb;
  s2.a = aa + 1;
  s2.b = bb + 1;

  /* A struct is not a valid argument for the condition selector.  */
  #pragma omp metadirective when(user={condition(s1)} : nothing) otherwise(nothing)  /* { dg-error "property must be integer expression" } */
  #pragma omp metadirective when(user={condition(s2)} : nothing) otherwise(nothing)  /* { dg-error "property must be integer expression" } */
}
