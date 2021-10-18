/* { dg-do compile } */

int x;
const int y = 0;
int bar(void);

void f1(void)
{
  #pragma omp atomic
    x %= 2;		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    x = x + 1;
  #pragma omp atomic
    x = 1;		/* { dg-error "invalid form" } */
  #pragma omp atomic	/* { dg-error "read-only variable" } */
    ++y;		/* { dg-error "read-only variable" } */
  #pragma omp atomic	/* { dg-error "read-only variable" } */
    y--;		/* { dg-error "read-only variable" } */
  #pragma omp atomic	/* { dg-error "read-only variable" } */
    y += 1;
  #pragma omp atomic
    bar();		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    bar() += 1;		/* { dg-error "lvalue required" } */
  #pragma omp atomic a	/* { dg-error "expected end of line" } */
    x++;		/* { dg-error "expected 'read', 'write', 'update', 'capture', 'compare', 'weak', 'fail', 'seq_cst', 'acq_rel', 'release', 'relaxed' or 'hint' clause" "" { target *-*-* } .-1 } */
  #pragma omp atomic
    ;			/* { dg-error "expected primary-expression" } */
  #pragma omp atomic
  #pragma omp atomic	/* { dg-error "not allowed" } */
    ;
  /* Check that we didn't get stuck on the pragma eol marker.  */
  undef;		/* { dg-error "" } */
}
