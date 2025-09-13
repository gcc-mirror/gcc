/* Test C2y constraint against negative array indices does not apply in
   C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a[1], b[10];
struct s { int a[2]; } x;
void *p;

void
f ()
{
  (void) a[0];
  (void) a[1];
  (void) a[12345];
  (void) a[-1];
  (void) a[-__LONG_LONG_MAX__];
  (void) b[0];
  (void) b[10];
  (void) b[12345];
  (void) b[-1];
  (void) b[-__LONG_LONG_MAX__];
  (void) x.a[0];
  (void) x.a[1];
  (void) x.a[12345];
  (void) x.a[-1];
  (void) x.a[-__LONG_LONG_MAX__];
  int c[1];
  (void) c[0];
  (void) c[1];
  (void) c[12345];
  (void) c[-1];
  (void) c[-__LONG_LONG_MAX__];
  (void) (*(int (*)[1]) p)[0];
  (void) (*(int (*)[1]) p)[1];
  (void) (*(int (*)[1]) p)[12345];
  (void) (*(int (*)[1]) p)[-1];
  (void) (*(int (*)[1]) p)[-__LONG_LONG_MAX__];
  /* This index is not an integer constant expression, so the constraint
     against negative indices does not apply even in C2y.  */
  (void) a[__LONG_LONG_MAX__ + 2];
  /* { dg-warning "integer overflow in expression" "overflow" { target *-*-* } .-1 } */
  /* Likewise, this is only an arithmetic constant expression, not an integer
     constant expression.  */
  (void) a[(int)-1.0];
}
