/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","123456"))) void
test (void)	/* { dg-error "\\\'123456\\\' is not supported in target attribute" } */
{}
