/* { dg-do compile } */

int x;
double d;

double
foo (int y, double e, long double f)
{
  double v;
  #pragma omp atomic compare compare	/* { dg-error "too many 'compare' clauses" } */
  x = x > y ? y : x;
  #pragma omp atomic compare fail(seq_cst) fail(seq_cst)	/* { dg-error "too many 'fail' clauses" } */
  d = e > d ? e : d;
  #pragma omp atomic compare,fail(seq_cst),fail(relaxed)	/* { dg-error "too many 'fail' clauses" } */
  d = e > d ? e : d;
  #pragma omp atomic compare weak weak	/* { dg-error "too many 'weak' clauses" } */
  d = d == e ? e + 1.0 : d;
  #pragma omp atomic read capture	/* { dg-error "'capture' clause is incompatible with 'read' or 'write' clauses" } */
  v = d;
  #pragma omp atomic capture, write	/* { dg-error "'capture' clause is incompatible with 'read' or 'write' clauses" } */
  d = v;
  #pragma omp atomic read compare	/* { dg-error "'compare' clause is incompatible with 'read' or 'write' clauses" } */
  v = d;
  #pragma omp atomic compare, write	/* { dg-error "'compare' clause is incompatible with 'read' or 'write' clauses" } */
  d = v;
  #pragma omp atomic read fail(seq_cst)	/* { dg-error "'fail' clause requires 'compare' clause" } */
  v = d;
  #pragma omp atomic fail(relaxed), write	/* { dg-error "'fail' clause requires 'compare' clause" } */
  d = v;
  #pragma omp atomic fail(relaxed) update	/* { dg-error "'fail' clause requires 'compare' clause" } */
  d += 3.0;
  #pragma omp atomic fail(relaxed)	/* { dg-error "'fail' clause requires 'compare' clause" } */
  d += 3.0;
  #pragma omp atomic capture fail(relaxed)	/* { dg-error "'fail' clause requires 'compare' clause" } */
  v = d += 3.0;
  #pragma omp atomic read weak		/* { dg-error "'weak' clause requires 'compare' clause" } */
  v = d;
  #pragma omp atomic weak, write	/* { dg-error "'weak' clause requires 'compare' clause" } */
  d = v;
  #pragma omp atomic weak update	/* { dg-error "'weak' clause requires 'compare' clause" } */
  d += 3.0;
  #pragma omp atomic weak		/* { dg-error "'weak' clause requires 'compare' clause" } */
  d += 3.0;
  #pragma omp atomic capture weak	/* { dg-error "'weak' clause requires 'compare' clause" } */
  v = d += 3.0;
  #pragma omp atomic compare,weak	/* { dg-error "'weak' clause requires atomic equality comparison" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail	/* { dg-error "expected '\\\(' before end of line" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail(	/* { dg-error "expected 'seq_cst', 'acquire' or 'relaxed' before end of line" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail()	/* { dg-error "expected 'seq_cst', 'acquire' or 'relaxed' before '\\\)' token" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail(foobar)	/* { dg-error "expected 'seq_cst', 'acquire' or 'relaxed' before 'foobar'" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail(acq_rel)	/* { dg-error "expected 'seq_cst', 'acquire' or 'relaxed' before 'acq_rel'" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail(release)	/* { dg-error "expected 'seq_cst', 'acquire' or 'relaxed' before 'release'" } */
  d = e > d ? e : d;
  #pragma omp atomic compare fail(seq_cst	/* { dg-error "expected '\\\)' before end of line" } */
  d = e > d ? e : d;
  return v;
}
