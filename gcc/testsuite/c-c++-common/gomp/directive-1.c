int thr;
#pragma omp threadprivate, (thr)		/* { dg-error "expected '\\\(' before ',' token" } */
						/* { dg-error "expected end of line before ',' token" "" { target c++ } .-1 } */
#pragma omp declare reduction, (foo: int : omp_out += omp_in), initializer (omp_priv = 0)	/* { dg-error "expected '\\\(' before ',' token" } */
void f1 (void);
#pragma omp declare variant, (f1), match (user={condition(true)})	/* { dg-error "expected '\\\(' before ',' token" } */
void f2 (void);
int j;
#pragma omp declare target, (j)			/* { dg-error "expected end of line before ',' token" } */

typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;
extern omp_depend_t d;

void
foo (void)
{
  int i, k = 0, l = 0;
  #pragma omp allocate, (i)			/* { dg-error "expected '\\\(' before ',' token" } */
						/* { dg-error "expected end of line before ',' token" "" { target c++ } .-1 } */
						/* { dg-message "not yet supported" "" { target *-*-* } .-2 } */
  #pragma omp critical, (bar)			/* { dg-error "expected '#pragma omp' clause before '\\\(' token" } */
  ;
  #pragma omp flush, (k, l)			/* { dg-error "expected '\\\(' or end of line before ',' token" "" { target c } } */
						/* { dg-error "expected end of line before ',' token" "" { target c++ } .-1 } */
  #pragma omp depobj, (d) depend(in : l)	/* { dg-error "expected '\\\(' before ',' token" } */
}
