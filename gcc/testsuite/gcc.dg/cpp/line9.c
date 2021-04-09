/* Test #line overflow checks: bug 97602.  */
/* { dg-do preprocess } */
/* { dg-options "-pedantic" } */

#line 5000000000 /* { dg-warning "line number out of range" } */
