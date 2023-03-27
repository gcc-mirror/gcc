// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-g -O2 -fopenmp -fkeep-inline-functions" }

int q;
struct A
{
  typedef int T;
#pragma omp declare reduction (x : T : omp_out += omp_in + [] (){ return q; }()) initializer (omp_priv = [](){ return 0; }())
  static void foo ();
};
void bar (int &, int &);
void
A::foo ()
{
  int r = 0, s = 0;
#pragma omp parallel reduction (x : r, s)
  bar (r, s);
}
