/* Test for rejection of __func__ outside a function (GNU extensions
   are OK there).  Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

__extension__ const char *a = __func__; /* { dg-error "'__func__' is not defined outside of function scope" "undef" } */
__extension__ const char *b = __FUNCTION__;
__extension__ const char *c = __PRETTY_FUNCTION__;
