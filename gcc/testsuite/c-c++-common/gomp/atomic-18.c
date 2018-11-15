int i, v;
float f;

void
foo (int j)
{
  #pragma omp atomic update,update	/* { dg-error "too many atomic clauses" } */
  i = i + 1;
  #pragma omp atomic seq_cst release	/* { dg-error "too many memory order clauses" } */
  i = i + 1;
  #pragma omp atomic read,release	/* { dg-error "incompatible with 'acq_rel' or 'release' clauses" } */
  v = i;
  #pragma omp atomic acq_rel read	/* { dg-error "incompatible with 'acq_rel' or 'release' clauses" } */
  v = i;
  #pragma omp atomic write acq_rel	/* { dg-error "incompatible with 'acq_rel' or 'acquire' clauses" } */
  i = v;
  #pragma omp atomic acquire , write	/* { dg-error "incompatible with 'acq_rel' or 'acquire' clauses" } */
  i = v;
  #pragma omp atomic update ,acquire	/* { dg-error "incompatible with 'acq_rel' or 'acquire' clauses" } */
  i = i + 1;
  #pragma omp atomic acq_rel update	/* { dg-error "incompatible with 'acq_rel' or 'acquire' clauses" } */
  i = i + 1;
  #pragma omp atomic acq_rel,hint(0)	/* { dg-error "incompatible with 'acq_rel' or 'acquire' clauses" } */
  i = i + 1;
  #pragma omp atomic acquire		/* { dg-error "incompatible with 'acq_rel' or 'acquire' clauses" } */
  i = i + 1;
  #pragma omp atomic capture hint (0) capture	/* { dg-error "too many atomic clauses" } */
  v = i = i + 1;
  #pragma omp atomic hint(j + 2)	/* { dg-error "constant integer expression" } */
  i = i + 1;
  #pragma omp atomic hint(f)		/* { dg-error "integ" } */
  i = i + 1;
  #pragma omp atomic foobar		/* { dg-error "expected 'read', 'write', 'update', 'capture', 'seq_cst', 'acq_rel', 'release', 'relaxed' or 'hint' clause" } */
  i = i + 1;				/* { dg-error "expected end of line before" "" { target *-*-* } .-1 } */
}
