/* Test case for PR 22231.  -c and -MG are invalid together.  */

/* { dg-do compile } */
/* { dg-options "-MG -MD -c" } */
/* { dg-error "may only be used with -M" "" 0 */

int anything;
