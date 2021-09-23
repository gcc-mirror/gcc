int a, b[64];
struct S { int c; } *d, *e;
struct T;
struct T *f, *g;
int *h;

void
f1 (void)
{
  #pragma omp task affinity (iterator : a)
  ;
  /* { dg-error "'iterator' undeclared " "" { target c } .-2 } */
  /* { dg-error "found ':' in nested-name-specifier, expected '::'" "" { target c++ } .-3 } */
  /* { dg-error "'iterator' has not been declared" "" { target c++ } .-4 } */
}

void
f2 (void)
{
  #pragma omp task affinity (iterator (for = 0 : 2) : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity (iterator (5 = 0 : 2) : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity (iterator (i : 0 : 2) : a)	/* { dg-error "expected '='|name a type|expected" } */
  ;
  #pragma omp task affinity (iterator (i = 0, 1 : 2) : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity (iterator (i = (0, 1) : 2) : a)
  ;
  #pragma omp task affinity (iterator (i = 0 : 1 : 2 : 3) : a)	/* { dg-error "expected '.'" } */
  ;
  #pragma omp task affinity (iterator (i = 0 : 2, 3) : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity (iterator (i = 0 : 10 : 2, 3) : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task affinity (iterator (i = 0:1), iterator (j = 0:1) : a)
  ;
  /* { dg-error "'iterator' undeclared " "" { target c } .-2 } */
  /* { dg-error "'i' was not declared in this scope" "" { target c++ } .-3 } */
  /* { dg-error "'iterator' was not declared in this scope" "" { target c++ } .-4 } */
  /* { dg-error "'j' was not declared in this scope" "" { target c++ } .-5 } */
  /* { dg-error "expected '\\)' before ':' token" "" { target c++ } .-6 } */
}

void
f3 (void)
{
  #pragma omp task affinity (iterator (i = 0:32) : b[i*2:2])
  ;
  #pragma omp task affinity (iterator (struct S i = 0:1) : a)		/* { dg-error "iterator 'i' has neither integral nor pointer type" } */
  ;
  #pragma omp task affinity (iterator (void i = 0:1) : a)		/* { dg-error "iterator 'i' has neither integral nor pointer type" } */
  ;
  #pragma omp task affinity (iterator (float f = 0.2:0.4) : a)	/* { dg-error "iterator 'f' has neither integral nor pointer type" } */
  ;
  #pragma omp task affinity (iterator (struct S *p = d:e:2) : a)
  ;
  #pragma omp task affinity (iterator (struct T *p = f:g) , a)
  ;
  /* { dg-error "'iterator' undeclared " "" { target c } .-2 } */
  /* { dg-error "expected primary-expression before 'struct'" "" { target c++ } .-3 } */
  /* { dg-error "'iterator' was not declared in this scope" "" { target c++ } .-4 } */
}

void
f4 (void)
{
  #pragma omp task affinity (iterator (int i = 0:4, \
				     struct U { int (*p)[i + 2]; } *p = 0:2) : a)	/* { dg-error "type of iterator 'p' refers to outer iterator 'i'" "" { target c } } */
  ;									/* { dg-error "types may not be defined in iterator type|not an integral constant" "" { target c++ } .-1 } */
  #pragma omp task affinity (iterator (i = 0:4, j = i:16) : a)	/* { dg-error "begin expression refers to outer iterator 'i'" } */
  ;
  #pragma omp task affinity (iterator (i = 0:4, j = 2:i:1) : a)	/* { dg-error "end expression refers to outer iterator 'i'" } */
  ;
  #pragma omp task affinity (iterator (i = 0:4, j = 2:8:i) : a)	/* { dg-error "step expression refers to outer iterator 'i'" } */
  ;
  #pragma omp task affinity (iterator (i = *d:2)  : a)	/* { dg-error "aggregate value used where an integer was expected" "" { target c } } */
  ;								/* { dg-error "invalid cast from type 'S' to type 'int'" "" { target c++ } .-1 } */
  #pragma omp task affinity (iterator (i = 2:*d:2) : a)	/* { dg-error "aggregate value used where an integer was expected" "" { target c } } */
  ;								/* { dg-error "invalid cast from type 'S' to type 'int'" "" { target c++ } .-1 } */
  #pragma omp task affinity (iterator (i = 2:4:*d) : a)	/* { dg-error "iterator step with non-integral type" } */
  ;
  #pragma omp task affinity (iterator (i = 1.25:2.5:3) : a)
  ;
  #pragma omp task affinity (iterator (i = 1:2:3.5) : a)	/* { dg-error "iterator step with non-integral type" } */
  ;
  #pragma omp task affinity (iterator (int *p = 23 : h) : a)
  ;
  #pragma omp task affinity (iterator (short i=1:3:0) : a)	/* { dg-error "iterator 'i' has zero step" } */
  ;
  #pragma omp task affinity (iterator (i = 1 : 3 : 3 - 3) : a)	/* { dg-error "iterator 'i' has zero step" } */
  ;
  #pragma omp task affinity (iterator (int *p = &b[6]:&b[9]:4 - 4) : a)	/* { dg-error "iterator 'p' has zero step" } */
  ;
  #pragma omp task affinity (iterator (const int i = 0 : 2) : a)	/* { dg-error "const qualified" } */
  ;
  #pragma omp task affinity (iterator (const long long unsigned i = 0 : 2) : a)	/* { dg-error "const qualified" } */
  ;
#if !defined (__cplusplus) && __STDC_VERSION__ >= 201112L
  #pragma omp task affinity (iterator (_Atomic unsigned i = 0 : 2) : a)	/* { dg-error "_Atomic" "" { target c } } */
  ;
#endif
}
