// PR c++/35244
// { dg-do compile }
// { dg-require-effective-target tls_native }
// { dg-options "-fopenmp" }

int v1;
namespace N1
{
  int v2;
}
namespace N2
{
  int v3;
}
using N1::v2;
using namespace N2;
struct A;
typedef int i;
#pragma omp threadprivate (i)	// { dg-error "is not file, namespace or block scope variable" }
#pragma omp threadprivate (A)	// { dg-error "is not file, namespace or block scope variable" }
#pragma omp threadprivate (v1, v2, v3)

void foo ()
{
  static int v4;
  {
    static int v5;
#pragma omp threadprivate (v4, v5)
  }
}
