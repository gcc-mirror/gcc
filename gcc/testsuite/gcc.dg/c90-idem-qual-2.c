/* Test for idempotent type qualifiers: in C99 only.  Test "const const".  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

const const int foo; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "duplicate" "duplicate type qualifier error" { target *-*-* } 6 } */
