/* Test diagnostic for an empty source file.  Test with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

/* { dg-warning "ISO C forbids an empty source file" "empty" { target *-*-* } 6 } */
