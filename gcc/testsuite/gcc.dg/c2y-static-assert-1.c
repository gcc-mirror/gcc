/* Test C2y static assertions with expressions that are not integer constant
   expressions (taken from c11-static-assert-3.c; in C2y these are constraint
   violations, whereas in C11 we diagnose them but they are undefined behavior
   because the requirement to be an integer constant expression is in Semantics
   not Constraints).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

_Static_assert (__INT_MAX__ * 2, "overflow"); /* { dg-warning "integer overflow in expression" } */
/* { dg-error "overflow in constant expression" "error" { target *-*-* } .-1 } */

_Static_assert ((void *)(__SIZE_TYPE__)16, "non-integer"); /* { dg-error "not an integer" } */

_Static_assert (1.0, "non-integer"); /* { dg-error "not an integer" } */

_Static_assert ((int)(1.0 + 1.0), "non-constant-expression"); /* { dg-error "not an integer constant expression" } */

int i;

_Static_assert (i, "non-constant"); /* { dg-error "not constant" } */
