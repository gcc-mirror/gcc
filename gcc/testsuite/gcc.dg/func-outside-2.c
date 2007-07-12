/* Test for rejection of __func__ outside a function (GNU extensions
   are OK there).  Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

const char *a = __func__; /* { dg-error "'__func__' is not defined outside of function scope" "undef" } */
const char *b = __FUNCTION__;
const char *c = __PRETTY_FUNCTION__;
