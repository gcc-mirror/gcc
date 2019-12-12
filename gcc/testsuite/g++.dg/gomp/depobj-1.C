typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

omp_depend_t bar (void);
extern const omp_depend_t cdepobj;
extern omp_depend_t depobj, depobj4;
extern omp_depend_t depobja[4];
extern omp_depend_t *pdepobj;
int a, b, i, j;

template <int N>
void
f1 (bool x)
{
  #pragma omp depobj(x ? depobj : depobj4) depend(in : x ? a : b)
  #pragma omp depobj(x ? depobj : depobj4) update(inout)
  #pragma omp task depend (depobj:depobj)
  ;
  #pragma omp depobj(depobj) destroy
  #pragma omp task depend (iterator (i=1:3) , depobj: *(depobja + i))
  ;
  #pragma omp depobj(pdepobj[0]) depend(mutexinoutset:a)
  #pragma omp depobj(*pdepobj) destroy
}

template <typename T, typename T2>
void
f2 (T &depobj2, T2 depobj3, T *pdepobj)
{
  T depobj1;
  T depobja[4];
  #pragma omp depobj(depobj1) depend(in : --a)
  #pragma omp depobj(depobj1) update(inout)
  #pragma omp task depend (depobj: depobj1)
  ;
  #pragma omp depobj(depobj1) destroy
  #pragma omp depobj(depobj2) depend(in : a)
  #pragma omp depobj(depobj2) update(inout)
  #pragma omp task depend (depobj :depobj2)
  ;
  #pragma omp depobj(depobj2) destroy
  #pragma omp depobj(depobj3) depend(in : a)
  #pragma omp depobj(depobj3) update(inout)
  #pragma omp task depend (depobj  :  depobj3)
  ;
  #pragma omp depobj(depobj3) destroy
  for (int q = 1; q < 3; q++)
    {
      #pragma omp depobj(depobja[q]) depend (in:a)
    }
  #pragma omp task depend (iterator (i=1:3) , depobj : *(depobja + i))
  ;
  for (int q = 1; q < 3; q++)
    {
      #pragma omp depobj(depobja[q]) destroy
    }
  #pragma omp depobj(pdepobj[0]) depend(mutexinoutset:a)
  #pragma omp depobj(*pdepobj) destroy
}

void
f3 (bool x)
{
  omp_depend_t depobjx, depobjy;
  f1 <0> (x);
  f2 <omp_depend_t, omp_depend_t &> (depobjx, depobjy, pdepobj);
}

template <int N>
void
f4 (void)
{
  omp_depend_t depobjb[4];
  #pragma omp depobj					// { dg-error "expected" }
  #pragma omp depobj destroy				// { dg-error "expected" }
  #pragma omp depobj (depobj)				// { dg-error "expected 'depend', 'destroy' or 'update' clause" }
  #pragma omp depobj (depobj) foobar			// { dg-error "expected 'depend', 'destroy' or 'update' clause" }
  #pragma omp depobj(bar ()) update(inout)		// { dg-error "'depobj' expression is not lvalue expression" }
  #pragma omp depobj (cdepobj) update(in)		// { dg-error "'const' qualified 'depobj' expression" }
  #pragma omp depobj (depobjb) depend(in: a)		// { dg-error "type of 'depobj' expression is not 'omp_depend_t'" }
  #pragma omp depobj (pdepobj) depend(in: a)		// { dg-error "type of 'depobj' expression is not 'omp_depend_t'" }
  #pragma omp depobj (a) destroy			// { dg-error "type of 'depobj' expression is not 'omp_depend_t'" }
  #pragma omp depobj (depobj) depend(depobj:a)		// { dg-error "does not have 'omp_depend_t' type in 'depend' clause with 'depobj' dependence type" }
  #pragma omp depobj (depobj) depend(depobj:*depobjb)	// { dg-error "'depobj' dependence type specified in 'depend' clause on 'depobj' construct" }
  #pragma omp depobj (depobj) update(foobar)		// { dg-error "expected 'in', 'out', 'inout' or 'mutexinoutset'" }
  #pragma omp depobj (depobj) depend(in: *depobja)	// { dg-error "should not have 'omp_depend_t' type in 'depend' clause with dependence type" }
  #pragma omp depobj (depobj) depend(in: a) depend(in: b)	// { dg-error "expected" }
  #pragma omp depobj (depobj) depend(in: a) update(out)	// { dg-error "expected" }
  #pragma omp depobj (depobj) depend(in: a, b)		// { dg-error "more than one locator in 'depend' clause on 'depobj' construct" }
  #pragma omp depobj (depobj) depend(source)		// { dg-error "'depend\\(source\\)' is only allowed in 'omp ordered'" }
  #pragma omp depobj (depobj) depend(sink: i + 1, j - 1)	// { dg-error "'depend\\(sink\\)' is only allowed in 'omp ordered'" }
  #pragma omp depobj (depobj) depend(iterator (i = 0:2) , in : a)	// { dg-error "'iterator' modifier may not be specified on 'depobj' construct" }
  if (0)
    #pragma omp depobj (depobj) destroy			// { dg-error "'#pragma omp depobj' may only be used in compound statements" }
    ;
}

template <int N>
void
f5 (void)
{
  #pragma omp task depend (depobj:depobja[1:2])		// { dg-error "'depend' clause with 'depobj' dependence type on array section" }
  ;
  #pragma omp task depend (depobj : a)			// { dg-error "'a' does not have 'omp_depend_t' type in 'depend' clause with 'depobj' dependence type" }
  ;
  #pragma omp task depend (in: depobj)			// { dg-error "'depobj' should not have 'omp_depend_t' type in 'depend' clause with dependence type" }
  ;
}

void
f6 (omp_depend_t &x)
{
  f4 <0> ();
  f5 <0> ();
  #pragma omp depobj (x) depend(in: a)
  #pragma omp depobj (depobj) depend(in: x)		// { dg-error "should not have 'omp_depend_t' type in 'depend' clause with dependence type" }
}
