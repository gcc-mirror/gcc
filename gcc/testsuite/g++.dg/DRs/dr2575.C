// DR 2575 - Undefined behavior when macro-replacing "defined" operator
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A defined
#if !A(A)		// { dg-error "this use of 'defined' may not be portable" }
#error
#endif
#if A(B)		// { dg-error "this use of 'defined' may not be portable" }
#error
#endif
#if !A A		// { dg-error "this use of 'defined' may not be portable" }
#error
#endif
#if A B			// { dg-error "this use of 'defined' may not be portable" }
#error
#endif
#if defined A + B
#else
#error
#endif
#if defined +B		// { dg-error "operator 'defined' requires an identifier" }
#endif			// { dg-error "missing binary operator before token 'B'" "" { target *-*-* } .-1 }
#if defined 1		// { dg-error "operator 'defined' requires an identifier" }
#endif
#if defined		// { dg-error "operator 'defined' requires an identifier" }
#endif
#if defined (A + B)	// { dg-error "missing '\\\)' after 'defined'" }
#endif			// { dg-error "missing binary operator before token 'B'" "" { target *-*-* } .-1 }
#if defined (+B)	// { dg-error "operator 'defined' requires an identifier" }
#endif			// { dg-error "missing binary operator before token 'B'" "" { target *-*-* } .-1 }
#if defined (1)		// { dg-error "operator 'defined' requires an identifier" }
#endif			// { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 }
#if defined ()		// { dg-error "operator 'defined' requires an identifier" }
#endif
#if defined A, B	// { dg-error "comma operator in operand of #if" }
#endif
#if defined (A), B	// { dg-error "comma operator in operand of #if" }
#endif
#if (defined A), B	// { dg-error "comma operator in operand of #if" }
#endif
#if defined (A, B)	// { dg-error "missing '\\\)' after 'defined'" }
#endif			// { dg-error "missing binary operator before token 'B'" "" { target *-*-* } .-1 }
#if defined (A) + B
#else
#error
#endif
#if (defined A) + B
#else
#error
#endif
