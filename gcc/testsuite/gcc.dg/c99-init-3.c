/* Test for designated initializers: array designators must be of
   integer type.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

int a[] = { [(void *)0] = 1 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "integer type|near init" "pointer designator"  { target *-*-* } 7 } */
