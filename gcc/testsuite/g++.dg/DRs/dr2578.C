// DR 2578 - Undefined behavior when creating an invalid string literal via stringizing
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A(a) #a
#define B(a) A(a)
#define C \\

const char *x = B(C);	// { dg-warning "invalid string literal, ignoring final '\\\\'" "" { target c++23_down } }
// { dg-error "invalid string literal, ignoring final '\\\\'" "" { target c++26 }  .-1 }
