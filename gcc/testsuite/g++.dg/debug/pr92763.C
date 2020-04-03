// { dg-do compile }
// { dg-require-effective-target fopenmp }
// { dg-additional-options "-fno-var-tracking-assignments -fopenmp" }

struct A
{
  typedef int T;
  #pragma omp declare reduction (y : T : [&omp_out, &omp_in]() { omp_out += omp_in; return 0; }()) initializer (omp_priv = [omp_orig]() { return omp_orig; }())
  static void foo ();
};

void
A::foo ()
{
  int r = 0, s = 0;
  #pragma omp parallel for reduction (y : r, s)
  for (int i = 0; i < 1; i++)
    {
    }
}
