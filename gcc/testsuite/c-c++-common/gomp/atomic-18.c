int i, v;
float f;

void
foo (int j)
{
  #pragma omp atomic update,update	/* { dg-error "too many atomic clauses" } */
  i = i + 1;
  #pragma omp atomic seq_cst release	/* { dg-error "too many memory order clauses" } */
  i = i + 1;
  #pragma omp atomic read,release	/* { dg-error "incompatible with 'release' clause" } */
  v = i;
  #pragma omp atomic acquire , write	/* { dg-error "incompatible with 'acquire' clause" } */
  i = v;
  #pragma omp atomic capture hint (0) capture	/* { dg-error "too many 'capture' clauses" } */
  v = i = i + 1;
  #pragma omp atomic hint(j + 2)	/* { dg-error "constant integer expression" } */
  i = i + 1;
  #pragma omp atomic hint(f)		/* { dg-error "integ" } */
  i = i + 1;
  #pragma omp atomic foobar		/* { dg-error "expected 'read', 'write', 'update', 'capture', 'compare', 'weak', 'fail', 'seq_cst', 'acq_rel', 'release', 'relaxed' or 'hint' clause" } */
  i = i + 1;				/* { dg-error "expected end of line before" "" { target *-*-* } .-1 } */
}
