/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","lsx;lasx"))) void
test (void)	/* { dg-error "in attribute \\\'lsx;lasx\\\', the second feature should be \\\"priority=\\\'num\\\'\\\" instead of \\\'lasx\\\'" } */
{}
