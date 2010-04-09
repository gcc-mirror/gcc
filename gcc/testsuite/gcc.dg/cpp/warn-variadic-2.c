/* { dg-do preprocess } */
/* { dg-options "-ansi -fdiagnostics-show-option -pedantic -Werror=variadic-macros" } */
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#define F(...) X   /* { dg-error "anonymous variadic macros were introduced in C99 .-Wvariadic-macros." } */

#define G(X...) X  /* { dg-error "ISO C does not permit named variadic macros .-Wvariadic-macros." } */
