/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","arch="))) void
test (void)	/* { dg-error "in attribute \\\'arch=\\\' you need to set a legal value for \\\"arch\\\"" } */
{}
