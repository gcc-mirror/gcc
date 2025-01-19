/* Test error-handling for legacy code that tries to
   define "bool" with C23.  */

/* { dg-do compile } */
/* { dg-options "-std=c23" } */

typedef int bool; /* { dg-error "'bool' cannot be defined via 'typedef'" } */
/* { dg-message "'bool' is a keyword with '-std=c23' onwards" "" { target *-*-* } .-1 } */
/* { dg-warning "useless type name in empty declaration"  "" { target *-*-* } .-2 } */

/* Also check that we report the correct standard version for "_Bool".  */
typedef int _Bool; /* { dg-error "'_Bool' cannot be defined via 'typedef'" } */
/* { dg-message "'_Bool' is a keyword with '-std=c99' onwards" "" { target *-*-* } .-1 } */
/* { dg-warning "useless type name in empty declaration"  "" { target *-*-* } .-2 } */
