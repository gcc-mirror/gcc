/* Test for integer constant types: diagnostics for constants outside
   range of intmax_t must be pedwarns.  PR 39559.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#if 9223372036854775808LL /* { dg-error "integer constant is so large that it is unsigned" } */
unsigned long long l = 9223372036854775808LL; /* { dg-error "integer constant is so large that it is unsigned" } */
#endif
