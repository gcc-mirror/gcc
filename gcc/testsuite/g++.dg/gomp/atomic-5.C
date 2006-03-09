/* { dg-do compile } */

int x;
const int y = 0;
int bar(void);

void f1(void)
{
  #pragma omp atomic
    x %= 2;		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    x = x + 1;		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    x = 1;		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    ++y;		/* { dg-error "read-only variable" } */
  #pragma omp atomic
    y--;		/* { dg-error "read-only variable" } */
  #pragma omp atomic
    y += 1;		/* { dg-error "read-only variable" } */
  #pragma omp atomic
    bar();		/* { dg-error "invalid operator" } */
  #pragma omp atomic
    bar() += 1;		/* { dg-error "lvalue required" } */
  #pragma omp atomic a	/* { dg-error "expected end of line" } */
    x++;
  #pragma omp atomic
    ;			/* { dg-error "expected primary-expression" } */
  #pragma omp atomic
  #pragma omp atomic	/* { dg-error "not allowed" } */
    ;
  /* Check that we didn't get stuck on the pragma eol marker.  */
  undef;		/* { dg-error "" } */
}
