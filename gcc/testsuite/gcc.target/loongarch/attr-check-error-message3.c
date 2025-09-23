/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default",";priority=1"))) void
test (void)	/* { dg-error "characher before \\\';\\\' in attribute \\\';priority=1\\\' cannot be empty" } */
{}
