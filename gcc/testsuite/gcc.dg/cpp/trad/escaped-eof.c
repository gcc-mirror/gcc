/* Test for warning of escaped EOF.  */

/* { dg-do preprocess } */

/* { dg-warning "backslash-new" "escaped EOF warning" { target *-*-* } 7 } */
\
