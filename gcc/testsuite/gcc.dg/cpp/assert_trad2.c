/* Malformed assertion tests.  */
/* { dg-do preprocess } */
/* { dg-options "-traditional" } */

#assert		/* { dg-error "without predicate" "assert w/o predicate" } */
#assert %	/* { dg-error "an identifier" "assert punctuation" } */
#assert 12	/* { dg-error "an identifier" "assert number" } */
#assert abc	/* { dg-error "missing" "assert w/o answer" } */

#if #		/* { dg-error "without predicate" "test w/o predicate" } */
#endif

#if #%		/* { dg-error "an identifier" "test punctuation" } */
#endif		/* { dg-error "(parse|syntax) error" "" { target *-*-* } 13 } */

#if #12		/* { dg-error "an identifier" "test number" } */
#endif		/* { dg-error "(parse|syntax) error" "" { target *-*-* } 16 } */

#if #abc
#error		/* { dg-bogus "error" "test w/o answer" } */
#endif

#if #abc[def]	/* { dg-error "(parse|syntax) error" "bad syntax" } */
#endif
