typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

omp_depend_t bar (void);
extern const omp_depend_t cdepobj;
extern omp_depend_t depobj;
extern omp_depend_t depobja[4];
extern omp_depend_t *pdepobj;
int a, b, i, j;

void
f1 (void)
{
  #pragma omp depobj(depobj) depend(in : a)
  #pragma omp depobj(depobj) update(inout)
  #pragma omp task depend (depobj: depobj)
  ;
  #pragma omp depobj(depobj) destroy
  #pragma omp task depend (iterator (i=1:3) , depobj: *(depobja + i))
  ;
  #pragma omp depobj(pdepobj[0]) depend(mutexinoutset:a)
  #pragma omp depobj(*pdepobj) destroy
}

void
f2 (void)
{
  omp_depend_t depobjb[4];
  #pragma omp depobj					/* { dg-error "expected" } */
  #pragma omp depobj destroy				/* { dg-error "expected" } */
  #pragma omp depobj (depobj)				/* { dg-error "expected 'depend', 'destroy' or 'update' clause" } */
  #pragma omp depobj (depobj) foobar			/* { dg-error "expected 'depend', 'destroy' or 'update' clause" } */
  #pragma omp depobj(bar ()) update(inout)		/* { dg-error "'depobj' expression is not lvalue expression" } */
  #pragma omp depobj (cdepobj) update(in)		/* { dg-error "'const' qualified 'depobj' expression" } */
  #pragma omp depobj (depobjb) depend(in: a)		/* { dg-error "type of 'depobj' expression is not 'omp_depend_t'" } */
  #pragma omp depobj (pdepobj) depend(in: a)		/* { dg-error "type of 'depobj' expression is not 'omp_depend_t'" } */
  #pragma omp depobj (a) destroy			/* { dg-error "type of 'depobj' expression is not 'omp_depend_t'" } */
  #pragma omp depobj (depobj) depend(depobj:a)		/* { dg-error "does not have 'omp_depend_t' type in 'depend' clause with 'depobj' dependence type" } */
  #pragma omp depobj (depobj) depend(depobj:*depobjb)	/* { dg-error "'depobj' dependence type specified in 'depend' clause on 'depobj' construct" } */
  #pragma omp depobj (depobj) update(foobar)		/* { dg-error "expected 'in', 'out', 'inout' or 'mutexinoutset'" } */
  #pragma omp depobj (depobj) depend(in: *depobja)	/* { dg-error "should not have 'omp_depend_t' type in 'depend' clause with dependence type" } */
  #pragma omp depobj (depobj) depend(in: a) depend(in: b)	/* { dg-error "expected" } */
  #pragma omp depobj (depobj) depend(in: a) update(out)	/* { dg-error "expected" } */
  #pragma omp depobj (depobj) depend(in: a, b)		/* { dg-error "more than one locator in 'depend' clause on 'depobj' construct" } */
  #pragma omp depobj (depobj) depend(source)		/* { dg-error "'depend\\(source\\)' is only allowed in 'omp ordered'" } */
  #pragma omp depobj (depobj) depend(sink: i + 1, j - 1)	/* { dg-error "'depend\\(sink\\)' is only allowed in 'omp ordered'" } */
  #pragma omp depobj (depobj) depend(iterator (i = 0:2) , in : a)	/* { dg-error "'iterator' modifier may not be specified on 'depobj' construct" } */
  if (0)
    #pragma omp depobj (depobj) destroy			/* { dg-error "'#pragma omp depobj' may only be used in compound statements" } */
    ;
}

void
f3 (void)
{
  #pragma omp task depend (depobj: depobja[1:2])	/* { dg-error "'depend' clause with 'depobj' dependence type on array section" } */
  ;
  #pragma omp task depend (depobj: a)			/* { dg-error "'a' does not have 'omp_depend_t' type in 'depend' clause with 'depobj' dependence type" } */
  ;
  #pragma omp task depend (in: depobj)			/* { dg-error "'depobj' should not have 'omp_depend_t' type in 'depend' clause with dependence type" } */
  ;
}
