// { dg-do compile }
// { dg-require-effective-target tls_native }

#define thr threadprivate

int i;
#pragma omp thr (i)
namespace N
{
  int j;
#pragma omp thr (j)
};
struct S
{
  static int s;
#pragma omp thr (s)
};

int S::s = 5;

int
foo ()
{
  static int k;
#pragma omp thr (k)
  return k++ + S::s;
}
