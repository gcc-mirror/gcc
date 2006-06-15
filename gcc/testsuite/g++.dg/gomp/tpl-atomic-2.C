// { dg-do compile }

struct S { int x; } s;

// Make sure we detect errors on non-type-dependent things
// even when the templates are never instantiated.
template<typename T> void f1()
{
  #pragma omp atomic
  s += 1;		// { dg-error "invalid" }
}

template<typename T> void f2(float *f)
{
  #pragma omp atomic
  *f |= 1;		// { dg-error "invalid|evaluation" }
}

// Here the rhs is dependent, but not type dependent.
template<typename T> void f3(float *f)
{
  #pragma omp atomic
  *f |= sizeof (T);	// { dg-error "invalid|evaluation" }
}

// And the converse, no error here because we're never fed a T.
template<typename T> void f4(T *t)
{
  #pragma omp atomic
  *t += 1;
}

// Here we'll let it go, because the rhs is type dependent and
// we can't properly instantiate the statement, and we do most
// of the semantic analysis concurrent with that.
template<typename T> void f5(float *f)
{
  #pragma omp atomic
  *f |= (T)sizeof(T);	// { dg-error "invalid|evaluation" "" { xfail *-*-* } }
}
