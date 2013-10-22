/* Test C11 static assertions.  Invalid assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Static_assert (__INT_MAX__ * 2, "overflow"); /* { dg-warning "integer overflow in expression" } */
/* { dg-error "overflow in constant expression" "error" { target *-*-* } 5 } */

_Static_assert ((void *)(__SIZE_TYPE__)16, "non-integer"); /* { dg-error "not an integer" } */

_Static_assert (1.0, "non-integer"); /* { dg-error "not an integer" } */

_Static_assert ((int)(1.0 + 1.0), "non-constant-expression"); /* { dg-error "not an integer constant expression" } */

int i;

_Static_assert (i, "non-constant"); /* { dg-error "not constant" } */

void
f (void)
{
  int j = 0;
  for (_Static_assert (sizeof (struct s { int k; }), ""); j < 10; j++) /* { dg-error "loop initial declaration" } */
    ;
}

_Static_assert (1, 1); /* { dg-error "expected" } */

_Static_assert (1, ("")); /* { dg-error "expected" } */
