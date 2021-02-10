// PR middle-end/99007
// { dg-additional-options "-Wno-div-by-zero" }

template <typename T>
void
bar (T *)
{
  T s[0/0];
  #pragma omp teams distribute parallel for reduction(+:s)
  for (int i = 0; i < 8; i++)
    ;
}

void
foo (long *a)
{
  bar (a);
}
