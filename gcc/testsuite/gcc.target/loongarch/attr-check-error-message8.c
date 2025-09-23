/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default","cmodel=normal"))) void
test (void)	/* { dg-error "attribute \\\'cmodel=normal\\\' is not supported in \\\'target_version\\\' or \\\'target_clones\\\'" } */
{}
