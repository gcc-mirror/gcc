template<typename t>
t
foo()
{
  t var = 5;
  #pragma omp allocate(var) align(sizeof(t) + 1)  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
  return var;
}

int
b()
{
  return foo<float>();  /* { dg-message "required from here" } */
}
