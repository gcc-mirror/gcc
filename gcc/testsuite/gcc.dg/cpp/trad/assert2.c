/* Malformed assertion tests.  */
/* { dg-do preprocess } */
/* { dg-options "-Wno-deprecated" } */

#assert		/* { dg-error "without predicate" "assert w/o predicate" } */
#assert %	/* { dg-error "an identifier" "assert punctuation" } */
#assert 12	/* { dg-error "an identifier" "assert number" } */
#assert abc	/* { dg-error "missing" "assert w/o answer" } */

#if #		/* { dg-error "without predicate" "test w/o predicate" } */
#endif

#if #%		/* { dg-error "an identifier" "test punctuation" } */
#endif

#if #12		/* { dg-error "an identifier" "test number" } */
#endif

#if #abc
#error		/* { dg-bogus "error" "test w/o answer" } */
#endif

#if #abc[def]	/* { dg-error "not valid in" "bad syntax" } */
#endif
