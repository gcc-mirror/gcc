/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

__attribute__ ((target ("mno-lsx"))) void
test1 (void)	/* { dg-error "attribute \\\'target\\\' argument \\\'mno-lsx\\\' is unknown" } */
{}

__attribute__ ((target (""))) void
test2 (void)	/* { dg-error "malformed \\\'target\\\(\\\)\\\' pragma or attribute" } */
{}

__attribute__ ((target ("no-cmodel="))) void
test3 (void)	/* { dg-error "pragma or attribute \\\'target\\\(\\\"cmodel\\\"\\\)\\\' does not allow a negated form" } */
{}

__attribute__ ((target ("cmodel=test"))) void
test4 (void)	/* { dg-error "pragma or attribute \\\'target\\\(\\\"cmodel=test\\\"\\\)\\\' is not valid" } */
{}

__attribute__ ((target ("test"))) void
test5 (void)	/* { dg-error "attribute \\\'target\\\' argument \\\'test\\\' is unknown" } */
{}

__attribute__ ((target (lsx))) void 	/* { dg-error "\\\'lsx\\\' undeclared here" } */
test6 (void)	/* { dg-error "attribute \\\'target\\\' argument not a string" } */
{}

__attribute__ ((target ("lsx,"))) void
test7 (void)	/* { dg-error "malformed \\\'target\\\(\\\"lsx,\\\"\\\)\\\' pragma or attribute" } */
{}
