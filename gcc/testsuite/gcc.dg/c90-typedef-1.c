/* Test typedef redeclaration not permitted in C90.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

typedef int TI; /* { dg-message "previous declaration" } */
typedef int TI; /* { dg-error "redefinition of typedef" } */
