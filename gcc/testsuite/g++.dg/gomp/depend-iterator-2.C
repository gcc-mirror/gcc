int a, b[64];
struct S { int c; } *d, *e;
struct T;
struct T *f, *g;
int *h;

template <typename U, typename V, typename W, W N>
void
f1 ()
{
  #pragma omp task depend (iterator , in : a)	// { dg-error "expected" }
  ;
  #pragma omp task depend (iterator (for = 0 : 2) , in : a)	// { dg-error "expected" }
  ;
  #pragma omp task depend (iterator (5 = 0 : 2) , in : a)	// { dg-error "expected" }
  ;
  #pragma omp task depend (iterator (i : N : 2) , in : a)	// { dg-error "expected '='|name a type|expected" }
  ;
  #pragma omp task depend (iterator (i = 0, 1 : 2) , in : a)	// { dg-error "expected" }
  ;
  #pragma omp task depend (iterator (i = (0, 1) : 2) , in : a)
  ;
  #pragma omp task depend (iterator (i = 0 : 1 : 2 : 3) , in : a)	// { dg-error "expected '.'" }
  ;
  #pragma omp task depend (iterator (i = 0 : 2, 3) , in : a)	// { dg-error "expected" }
  ;
  #pragma omp task depend (iterator (i = N : 10 : 2, 3) , in : a)	// { dg-error "expected" }
  ;
  #pragma omp task depend (iterator (i = 0:1), iterator (j = 0:1) , in : a)	// { dg-error "invalid depend kind" }
  ;
  #pragma omp task depend (iterator (i = N:32) , in : b[i*2:2])
  ;
  #pragma omp task depend (iterator (void i = 0:1) , in : a)		// { dg-error "iterator 'i' has neither integral nor pointer type" }
  ;
  #pragma omp task depend (iterator (U *p = d:e:2) , in : a)
  ;
  #pragma omp task depend (iterator (W i = N:4, \
				     struct U2 { W *p; } *p = 0:2) , in : a) // { dg-error "types may not be defined in iterator type" }
  ;
  #pragma omp task depend (iterator (i = 0:4, j = i:16) , in : a)	// { dg-error "begin expression refers to outer iterator 'i'" }
  ;
  #pragma omp task depend (iterator (i = N:4, j = 2:i:1) , in : a)	// { dg-error "end expression refers to outer iterator 'i'" }
  ;
  #pragma omp task depend (iterator (i = 0:4, j = 2:8:i) , in : a)	// { dg-error "step expression refers to outer iterator 'i'" }
  ;
  #pragma omp task depend (iterator (i = 1.25:2.5:3) , in : a)
  ;
  #pragma omp task depend (iterator (i = 1:2:3.5) , in : a)		// { dg-error "iterator step with non-integral type" }
  ;
  #pragma omp task depend (iterator (W *p = 23 : h) , in : a)
  ;
  #pragma omp task depend (iterator (const int i = N : 2) , in : a)	// { dg-error "const qualified" }
  ;
  #pragma omp task depend (iterator (const long long unsigned i = 0 : 2) , in : a)	// { dg-error "const qualified" }
  ;
}

template <typename W, int N>
void
f2 ()
{
  int i, j;
  #pragma omp for ordered(2)
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      {
      #pragma omp ordered depend (iterator (k=0:N) , sink: i - 1, j - 1)	// { dg-error "'iterator' modifier incompatible with 'sink'" }
      #pragma omp ordered depend (iterator (W l = 0:2:3) , source)		// { dg-error "'iterator' modifier incompatible with 'source'" }
      }
}

template <typename U, typename V, typename W, W N, typename X, typename Y>
void
f3 ()
{
  #pragma omp task depend (iterator (U i = 0:1), in : a)		// { dg-error "iterator 'i' has neither integral nor pointer type" }
  ;
  #pragma omp task depend (iterator (V f = 0.2:0.4) , in : a)	// { dg-error "iterator 'f' has neither integral nor pointer type" }
  ;
  #pragma omp task depend (iterator (struct T *p = f:g) , in : a)	// { dg-error "invalid use of" }
  ;
  #pragma omp task depend (iterator (i = *d:2) , in : a)	// { dg-error "invalid cast from type 'S' to type 'int'" }
  ;
  #pragma omp task depend (iterator (i = 2:*d:2) , in : a)	// { dg-error "invalid cast from type 'S' to type 'int'" }
  ;
  #pragma omp task depend (iterator (i = 2:4:*d) , in : a)	// { dg-error "iterator step with non-integral type" }
  ;
  #pragma omp task depend (iterator (i = 1.25:2.5:3) , in : a)
  ;
  #pragma omp task depend (iterator (i = 1:2:3.5) , in : a)	// { dg-error "iterator step with non-integral type" }
  ;
  #pragma omp task depend (iterator (W *p = 23 : h) , in : a)
  ;
  #pragma omp task depend (iterator (short i=1:3:N) , in : a)	// { dg-error "iterator 'i' has zero step" }
  ;
  #pragma omp task depend (iterator (i = 1 : 3 : N + 3 - 3) , in : a)	// { dg-error "iterator 'i' has zero step" }
  ;
  #pragma omp task depend (iterator (int *p = &b[6]:&b[9]:4 - 4) , in : a)	// { dg-error "iterator 'p' has zero step" }
  ;
  #pragma omp task depend (iterator (X i = N : 2) , in : a)	// { dg-error "const qualified" }
  ;
  #pragma omp task depend (iterator (Y i = 0 : 2) , in : a)	// { dg-error "const qualified" }
  ;
}

template <int N>
void
f4 ()
{
  #pragma omp task depend (iterator (i = 0:1), iterator (j = 0:1) , in : a)	// { dg-error "invalid depend kind" }
  ;
}

void
f5 ()
{
  f1 <struct S, float, int, 0> ();
  f2 <int, 1> ();
  f3 <struct S, float, int, 0, const int, const long long unsigned> ();
  f4 <0> ();
}
