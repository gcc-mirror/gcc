/* Test #line overflow checks: bug 97602.  */
/* { dg-do preprocess } */
/* { dg-options "-pedantic" } */

#line 4294967296 /* { dg-warning "line number out of range" } */
