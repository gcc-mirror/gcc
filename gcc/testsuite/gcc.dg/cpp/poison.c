/* { dg-do preprocess }
   { dg-options "-fno-show-column" } */

#pragma GCC poison foo
foo			/* { dg-error "foo" "use of foo" } */
#pragma GCC poison foo2 foo3
foo2			/* { dg-error "foo2" "use of foo2" } */
foo3			/* { dg-error "foo3" "use of foo3" } */
#pragma GCC poison	foo4 	foo5
foo4			/* { dg-error "foo4" "use of foo4" } */
foo5			/* { dg-error "foo5" "use of foo5" } */
#pragma GCC poison +++	/* { dg-error "invalid" "poison non-identifier" } */
#define foo6 123
#pragma GCC poison foo6	/* { dg-warning "foo6" "poison defined macro" } */
#define foo6 345	/* { dg-error "foo6" "def of foo6" } */
#define foo6 456	/* { dg-error "foo6" "redef of foo6" } */
#ifdef foo6		/* { dg-error "foo6" "#ifdef foo6" } */
#error hey! foo6 defined!
#endif
#if defined(foo6)	/* { dg-error "foo6" "#if defined foo6" } */
#error foo6 still defined!
#else
foo6			/* { dg-error "foo6" "use of foo6" } */
#endif
#pragma GCC poison
