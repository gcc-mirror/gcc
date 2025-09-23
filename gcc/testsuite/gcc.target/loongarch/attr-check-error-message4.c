/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","lsx;priority=1;lasx"))) void
test (void)	/* { dg-error "in attribute \\\'lsx;priority=1;lasx\\\' the number of reatures cannot exceed two" } */
{}
