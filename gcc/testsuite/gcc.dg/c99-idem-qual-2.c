/* Test for idempotent type qualifiers: in C99 only.  Test "const const".  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

const const int foo; /* { dg-bogus "duplicate" "duplicate type qualifier error" } */
