/* Test the -fconstant-string-class flag error.  */
/* { dg-do compile } */
/* { dg-options "-fconstant-string-class" } */

{ dg-error "no class name specified as argument to -fconstant-string-class" "" { target *-*-* } 0 }

int foo () {}

/* Seem bogus, should investigate some day.  */
/* { dg-error "parse error" "" { target *-*-* } 5 } */
