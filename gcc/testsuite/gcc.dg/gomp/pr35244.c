/* PR c++/35244 */
/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-fopenmp" } */

int v1;
typedef struct A A;
typedef int i;
#pragma omp threadprivate (i)	/* { dg-error "expected identifier before" } */
#pragma omp threadprivate (A)	/* { dg-error "expected identifier before" } */
#pragma omp threadprivate (v1)

void foo ()
{
  static int v4;
  {
    static int v5;
#pragma omp threadprivate (v4, v5)
  }
}
