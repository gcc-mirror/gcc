/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

struct T { int c[3]; };
struct S { int a; struct T *b; struct T g; };
struct S d[10];
struct S *e[10];
struct S *f;
struct S h;

void
foo (void)
{
  #pragma omp task depend(inout: d)
  ;
  #pragma omp task depend(out: d[2])
  ;
  #pragma omp task depend(in: d[:])
  ;
  #pragma omp task depend(in: d[2:2])
  ;
  #pragma omp task depend(in: d[:2])
  ;
  #pragma omp task depend(inout: d[1].b->c[2])
  ;
  #pragma omp task depend(out: d[0].a)
  ;
  #pragma omp task depend(in: e[3]->a)
  ;
  #pragma omp task depend(inout: e[2]->b->c)
  ;
  #pragma omp task depend(in: e[1]->b->c[2])
  ;
  #pragma omp task depend(out: (*f).a)
  ;
  #pragma omp task depend(inout: f->b->c[0])
  ;
  #pragma omp task depend(in: f)
  ;
  #pragma omp task depend(out: *f)
  ;
  #pragma omp task depend(inout: f[0])
  ;
  #pragma omp task depend(in: f[0].a)
  ;
  #pragma omp task depend(inout: h.g.c[2])
  ;
}
