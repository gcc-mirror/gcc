/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","lsx;"))) void
test (void)	/* { dg-error "in attribute \\\'lsx;\\\' priority cannot be empty" } */
{}
