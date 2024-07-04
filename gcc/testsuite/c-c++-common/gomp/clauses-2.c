/* { dg-skip-if "PR 68733" { hppa*-*-hpux* && { ! lp64 } } } */
struct S { int r; int *s; int t[10]; };
void bar (int *);

void
foo (int *p, int q, struct S t, int i, int j, int k, int l)
{
  #pragma omp target map (q), firstprivate (q) /* { dg-error "appears both in data and map clauses" } */
    bar (&q);
  #pragma omp target map (p[0]) firstprivate (p) /* { dg-error "appears more than once in data clauses" } */
    bar (p);
  #pragma omp target firstprivate (p), map (p[0]) /* { dg-error "appears more than once in data clauses" } */
    bar (p);
  #pragma omp target map (p[0]) map (p)
    bar (p);
  #pragma omp target map (p) , map (p[0])
    bar (p);
  #pragma omp target map (q) map (q) /* { dg-error "appears more than once in map clauses" } */
    bar (&q);
  #pragma omp target map (p[0]) map (p[0]) /* { dg-error "appears more than once in data clauses" } */
    bar (p);
  #pragma omp target map (t) map (t.r)
    bar (&t.r);
  #pragma omp target map (t.r) map (t)
    bar (&t.r);
  #pragma omp target map (t.r) map (t.r)
    bar (&t.r);
  #pragma omp target firstprivate (t), map (t.r) /* { dg-error "appears both in data and map clauses" } */
    bar (&t.r);
  #pragma omp target map (t.r) firstprivate (t) /* { dg-error "appears both in data and map clauses" } */
    bar (&t.r);
  #pragma omp target map (t.s[0]) map (t)
    bar (t.s);
  #pragma omp target map (t) map(t.s[0])
    bar (t.s);
  #pragma omp target firstprivate (t) map (t.s[0]) /* { dg-error "appears both in data and map clauses" } */
    bar (t.s);
  #pragma omp target map (t.s[0]) firstprivate (t) /* { dg-error "appears both in data and map clauses" } */
    bar (t.s);
  #pragma omp target map (t.s[0]) map (t.s[2])
    bar (t.s);
  #pragma omp target map (t.t[0:2]) map (t.t[4:6])
    bar (t.t);
  #pragma omp target map (t.t[i:j]) map (t.t[k:l])
    bar (t.t);
  #pragma omp target map (t.s[0]) map (t.r)
    bar (t.s);
  #pragma omp target map (t.r) ,map (t.s[0])
    bar (t.s);
  #pragma omp target map (t.r) map (t) map (t.s[0]) firstprivate (t) /* { dg-error "appears both in data and map clauses" } */
    bar (t.s);
  #pragma omp target map (t) map (t.r) firstprivate (t) map (t.s[0]) /* { dg-error "appears both in data and map clauses" } */
    bar (t.s);
}
