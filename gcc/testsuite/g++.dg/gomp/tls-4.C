// { dg-do compile }
// { dg-require-effective-target tls_native }

#define thr threadprivate

struct S
{
  static int s;
};
struct T : public S
{
  static int t;
#pragma omp thr (s)	// { dg-error "directive not in" }
};

#pragma omp thr (T::t)	// { dg-error "directive not in" }
