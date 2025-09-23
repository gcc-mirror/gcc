/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","lsx;priority=-1"))) void
test (void)	/* { dg-error "Setting the priority value to \\\'-1\\\' is illegal in attribute \\\'lsx;priority=-1\\\'" } */
{}
