/* { dg-do preprocess } */
/* { dg-options "-ansi -fdiagnostics-show-option -pedantic -Wvariadic-macros" } */

#define F(...) X   /* { dg-warning "anonymous variadic macros were introduced in C99 .-Wvariadic-macros." } */

#define G(X...) X  /* { dg-warning "ISO C does not permit named variadic macros .-Wvariadic-macros." } */
