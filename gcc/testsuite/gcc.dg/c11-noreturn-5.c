/* Test C11 _Noreturn.  Test invalid uses.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Noreturn struct s; /* { dg-error "empty declaration" } */

typedef _Noreturn void f (void); /* { dg-error "typedef" } */

void g (_Noreturn void fp (void)); /* { dg-error "parameter" } */

_Noreturn void (*p) (void); /* { dg-error "variable" } */

struct t { int a; _Noreturn void (*f) (void); }; /* { dg-error "expected" } */

int *_Noreturn *q; /* { dg-error "expected" } */

int i = sizeof (_Noreturn int (*) (void)); /* { dg-error "expected" } */
