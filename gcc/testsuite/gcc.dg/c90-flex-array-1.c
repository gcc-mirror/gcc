/* Test for flexible array members.  Test for rejection in C90 mode.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct flex { int a; int b[]; }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "ISO C90" "flexible array members not in C90" { target *-*-* } 6 } */
