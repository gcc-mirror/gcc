/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target_clones ("default;priority=1","lsx"))) void
test (void)	/* { dg-error "\\\"default\\\" cannot be set together with other features in \\\'default;priority=1\\\'" } */
{}
