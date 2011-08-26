/* Test Unicode strings in C1X.  Test constraint.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

const void *p1 = L"a" u8"b"; /* { dg-error "concatenation" } */
const void *p2 = L"a" "b" u8"c"; /* { dg-error "concatenation" } */
const void *p3 = u8"a" L"b"; /* { dg-error "concatenation" } */
const void *p4 = u8"a" "b" L"c"; /* { dg-error "concatenation" } */
