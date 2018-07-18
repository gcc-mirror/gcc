/* Test for idempotent type qualifiers: in C99 only.  Test duplicate
   type qualifiers with array element types.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

typedef const int cia[2];
const cia a; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "duplicate" "duplicate type qualifier error" { target *-*-* } .-1 } */
const cia b[2]; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "duplicate" "duplicate type qualifier error" { target *-*-* } .-1 } */
