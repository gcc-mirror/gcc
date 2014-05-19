// PR c++/49223
// { dg-do compile }
// { dg-require-effective-target tls }
// { dg-options "-fopenmp" }

struct S;			// { dg-message "forward declaration" }
extern __thread struct S s;	// { dg-error "has incomplete type" }
struct T;
extern __thread struct T t;

void
foo ()
{
  #pragma omp parallel copyin (s)
    ;
}
