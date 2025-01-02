/* Test error-handling for legacy code that tries to
   define "false" or "true" within enums with C23.  */

/* { dg-do compile } */
/* { dg-options "-std=c23" } */

typedef enum { false = 0, true = 1 } _Bool; /* { dg-error "cannot use keyword 'false' as enumeration constant" }
/* { dg-message "'false' is a keyword with '-std=c23' onwards" "" { target *-*-* } .-1 } */
/* { dg-error "38: expected ';', identifier or '\\\(' before '_Bool'" "" { target *-*-* } .-2 } */
/* { dg-warning "38: useless type name in empty declaration" "" { target *-*-* } .-3 } */

typedef enum { true = 1, false = 0 } _Bool; /* { dg-error "cannot use keyword 'true' as enumeration constant" }
/* { dg-message "'true' is a keyword with '-std=c23' onwards" "" { target *-*-* } .-1 } */
/* { dg-error "38: expected ';', identifier or '\\\(' before '_Bool'" "" { target *-*-* } .-2 } */
/* { dg-warning "38: useless type name in empty declaration" "" { target *-*-* } .-3 } */

typedef enum { False = 0, True = 1 } bool; /* { dg-error "expected ';', identifier or '\\(' before 'bool'" }
/* { dg-warning "38: useless type name in empty declaration" "" { target *-*-* } .-1 } */
