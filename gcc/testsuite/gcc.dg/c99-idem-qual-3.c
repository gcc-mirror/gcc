/* Test for idempotent type qualifiers: in C99 only.  Test duplicate
   type qualifiers with array element types.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef const int cia[2];
const cia a; /* { dg-bogus "duplicate" "duplicate type qualifier warning" } */
const cia b[2]; /* { dg-bogus "duplicate" "duplicate type qualifier warning" } */
