/* Malformed assertion tests.  */
/* { dg-do preprocess } */
/* { dg-options "-fshow-column -Wno-deprecated" } */

#assert		/* { dg-error "without predicate" "assert w/o predicate" } */
#assert %	/* { dg-error "9:an identifier" "assert punctuation" } */
#assert 12	/* { dg-error "9:an identifier" "assert number" } */
#assert abc	/* { dg-error "9:missing" "assert w/o answer" } */

#if #		/* { dg-error "without predicate" "test w/o predicate" } */
#endif

#if #%		/* { dg-error "6:an identifier" "test punctuation" } */
#endif

#if #12		/* { dg-error "6:an identifier" "test number" } */
#endif

#if #abc
#error		/* { dg-bogus "error" "test w/o answer" } */
#endif

#if #abc[def]	/* { dg-error "9:is not valid" "test with malformed answer" } */
#endif
