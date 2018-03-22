/* PR c/66618 */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

int a = "foo"[2];
int b = 1["bar"];
int c = "baz"[__INT_MAX__ * -2];	/* { dg-error "initializer element is not constant" } */
int d = "str"[3];			/* { dg-warning "integer overflow in expression of type" "" { target *-*-* } .-1 } */
int e = "str"[4];			/* { dg-error "initializer element is not constant" } */
int f = "str"[-1];			/* { dg-error "initializer element is not constant" } */
