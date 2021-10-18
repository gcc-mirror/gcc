/* { dg-do compile } */

int x;
const int y;
int bar(void);

void f1(void)
{
  register int z;

  #pragma omp atomic
    x %= 2;		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    x = x + 1;
  #pragma omp atomic
    x = 1;		/* { dg-error "invalid form" } */
  #pragma omp atomic
    ++y;		/* { dg-error "read-only variable" } */
  #pragma omp atomic
    y--;		/* { dg-error "read-only variable" } */
  #pragma omp atomic
    y += 1;		/* { dg-error "read-only variable" } */
  #pragma omp atomic
    z += 1;		/* { dg-error "register variable" } */
  #pragma omp atomic
    bar();		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    bar() += 1;		/* { dg-error "lvalue required" } */
  #pragma omp atomic a	/* { dg-error "expected end of line" } */
    x++;		/* { dg-error "expected 'read', 'write', 'update', 'capture', 'compare', 'weak', 'fail', 'seq_cst', 'acq_rel', 'release', 'relaxed' or 'hint' clause" "" { target *-*-* } .-1 } */
  #pragma omp atomic
    ;			/* { dg-error "expected expression" } */
  #pragma omp atomic
  #pragma omp atomic	/* { dg-error "expected expression" } */
    ;
  /* Check that we didn't get stuck on the pragma eol marker.  */
  undef;		/* { dg-error "" } */
  /* { dg-message "undeclared identifier is reported only once" "reminder" { target *-*-* } .-1 } */
}
