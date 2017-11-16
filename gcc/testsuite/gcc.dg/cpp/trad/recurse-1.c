/* Test for warning of and recovery from recursion in object-like
   macros.  */

/* { dg-do preprocess } */

#define foo foo
foo				/* { dg-error "-:detected recursion" } */

#define bar a bar b
bar				/* { dg-error "-:detected recursion" } */
