/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

struct T { int c[3]; };
struct S { int a; struct T *b; struct T g; };
struct U { int a : 5; };
struct S d[10];
struct S *e[10];
struct S *f;
struct S h;
struct U i;

void
foo (void)
{
  #pragma omp task depend(in: d[:2].b->c[2])	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(inout: d[1:].b->c[2])	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(out: d[0:1].a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(in: e[3:2]->a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(inout: e[2:2]->b->c)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(in: e[1]->b->c[2:1])	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(out: f + 0)		/* { dg-error "not lvalue expression" } */
  ;
  #pragma omp task depend(inout: f[0:1].a)	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(inout: h.g.c[2:1])	/* { dg-error "expected" } */
  ;
  #pragma omp task depend(in: i.a)		/* { dg-error "bit-field '\[^\n\r]*' in 'depend' clause" } */
  ;
}
