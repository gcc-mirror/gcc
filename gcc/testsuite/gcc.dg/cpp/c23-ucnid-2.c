/* Test C23 rules for characters in identifiers: \u0024 not allowed.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -fdollars-in-identifiers -pedantic-errors" } */

\u0024 /* { dg-error "is not valid in an identifier" } */
