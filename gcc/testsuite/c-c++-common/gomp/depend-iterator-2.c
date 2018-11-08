int a, b[64];
struct S { int c; } *d, *e;
struct T;
struct T *f, *g;
int *h;

void
f1 (void)
{
  #pragma omp task depend (iterator , in : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend (iterator (for = 0 : 2) , in : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend (iterator (5 = 0 : 2) , in : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend (iterator (i : 0 : 2) , in : a)	/* { dg-error "expected '='|name a type|expected" } */
  ;
  #pragma omp task depend (iterator (i = 0, 1 : 2) , in : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend (iterator (i = (0, 1) : 2) , in : a)
  ;
  #pragma omp task depend (iterator (i = 0 : 1 : 2 : 3) , in : a)	/* { dg-error "expected '.'" } */
  ;
  #pragma omp task depend (iterator (i = 0 : 2, 3) , in : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend (iterator (i = 0 : 10 : 2, 3) , in : a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend (iterator (i = 0:1), iterator (j = 0:1) , in : a)	/* { dg-error "invalid depend kind" } */
  ;
  #pragma omp task depend (iterator (i = 0:32) , in : b[i*2:2])
  ;
  #pragma omp task depend (iterator (struct S i = 0:1), in : a)		/* { dg-error "iterator 'i' has neither integral nor pointer type" } */
  ;
  #pragma omp task depend (iterator (void i = 0:1) , in : a)		/* { dg-error "iterator 'i' has neither integral nor pointer type" } */
  ;
  #pragma omp task depend (iterator (float f = 0.2:0.4) , in : a)	/* { dg-error "iterator 'f' has neither integral nor pointer type" } */
  ;
  #pragma omp task depend (iterator (struct S *p = d:e:2) , in : a)
  ;
  #pragma omp task depend (iterator (struct T *p = f:g) , in : a)	/* { dg-error "invalid use of" } */
  ;
  #pragma omp task depend (iterator (int i = 0:4, \
				     struct U { int (*p)[i + 2]; } *p = 0:2) , in : a)	/* { dg-error "type of iterator 'p' refers to outer iterator 'i'" "" { target c } } */
  ;									/* { dg-error "types may not be defined in iterator type|not an integer constant" "" { target c++ } .-1 } */
  #pragma omp task depend (iterator (i = 0:4, j = i:16) , in : a)	/* { dg-error "begin expression refers to outer iterator 'i'" } */
  ;
  #pragma omp task depend (iterator (i = 0:4, j = 2:i:1) , in : a)	/* { dg-error "end expression refers to outer iterator 'i'" } */
  ;
  #pragma omp task depend (iterator (i = 0:4, j = 2:8:i) , in : a)	/* { dg-error "step expression refers to outer iterator 'i'" } */
  ;
  #pragma omp task depend (iterator (i = *d:2) , in : a)	/* { dg-error "aggregate value used where an integer was expected" "" { target c } } */
  ;								/* { dg-error "invalid cast from type 'S' to type 'int'" "" { target c++ } .-1 } */
  #pragma omp task depend (iterator (i = 2:*d:2) , in : a)	/* { dg-error "aggregate value used where an integer was expected" "" { target c } } */
  ;								/* { dg-error "invalid cast from type 'S' to type 'int'" "" { target c++ } .-1 } */
  #pragma omp task depend (iterator (i = 2:4:*d) , in : a)	/* { dg-error "iterator step with non-integral type" } */
  ;
  #pragma omp task depend (iterator (i = 1.25:2.5:3) , in : a)
  ;
  #pragma omp task depend (iterator (i = 1:2:3.5) , in : a)	/* { dg-error "iterator step with non-integral type" } */
  ;
  #pragma omp task depend (iterator (int *p = 23 : h) , in : a)
  ;
  #pragma omp task depend (iterator (short i=1:3:0) , in : a)	/* { dg-error "iterator 'i' has zero step" } */
  ;
  #pragma omp task depend (iterator (i = 1 : 3 : 3 - 3) , in : a)	/* { dg-error "iterator 'i' has zero step" } */
  ;
  #pragma omp task depend (iterator (int *p = &b[6]:&b[9]:4 - 4) , in : a)	/* { dg-error "iterator 'p' has zero step" } */
  ;
  #pragma omp task depend (iterator (const int i = 0 : 2) , in : a)	/* { dg-error "const qualified" } */
  ;
  #pragma omp task depend (iterator (const long long unsigned i = 0 : 2) , in : a)	/* { dg-error "const qualified" } */
  ;
#if !defined (__cplusplus) && __STDC_VERSION__ >= 201112L
  #pragma omp task depend (iterator (_Atomic unsigned i = 0 : 2) , in : a)	/* { dg-error "_Atomic" "" { target c } } */
  ;
#endif
}

void
f2 (void)
{
  int i, j;
  #pragma omp for ordered(2)
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      {
      #pragma omp ordered depend (iterator (k=0:1) , sink: i - 1, j - 1)	/* { dg-error "'iterator' modifier incompatible with 'sink'" } */
      #pragma omp ordered depend (iterator (int l = 0:2:3) , source)		/* { dg-error "'iterator' modifier incompatible with 'source'" } */
      }
}

void
f3 (void)
{
  #pragma omp task depend (iterator (i = 0:1), iterator (j = 0:1) , in : a)	/* { dg-error "invalid depend kind" } */
  ;
}
