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
#pragma omp thr (s) // { dg-error "is not file, namespace or block scope" }
};

int
foo ()
{
  static int k;
#pragma omp thr (k)
  return k++ + S::s;
}
