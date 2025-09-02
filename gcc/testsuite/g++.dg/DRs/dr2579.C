// DR 2579 - Undefined behavior when token pasting does not create a preprocessing token
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A(a, b) a ## b
A(5,6)
A(-,32)			// { dg-error "pasting '-' and '32' does not give a valid preprocessing token" }
A("","")		// { dg-error "pasting '\"\"' and '\"\"' does not give a valid preprocessing token" }
A(\,u0393)
