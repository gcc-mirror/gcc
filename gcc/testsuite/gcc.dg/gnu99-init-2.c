/* Test for designated initializers: array designators must be of
   integer type.  Test for index ranges (GNU extension).  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int a[] = { [(void *)0 ... 0] = 1 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "integer type|near init" "pointer designator"  { target *-*-* } 7 } */

int b[] = { [0 ... (void *)0] = 1 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "integer type|near init" "pointer designator"  { target *-*-* } 10 } */
