void
test_fopenacc ()
{
  __builtin_printf ("_OPENACC = %d\n", _OPENACC); /* { dg-error "'_OPENACC' undeclared" "" { target c } } */
  /* { dg-error "'_OPENACC' was not declared in this scope" "" { target c++ } .-1 } */
  /* { dg-message "'_OPENACC' is defined when using option '-fopenacc'" "" { target *-*-* } .-2 } */
}

void
test_fopenmp ()
{
  __builtin_printf ("_OPENMP = %d\n", _OPENMP); /* { dg-error "'_OPENMP' undeclared" "" { target c } } */
  /* { dg-error "'_OPENMP' was not declared in this scope" "" { target c++ } .-1 } */
  /* { dg-message "'_OPENMP' is defined when using option '-fopenmp'" "" { target *-*-* } .-2 } */
}
