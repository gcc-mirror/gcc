/* Test C23 constexpr.  Invalid types of integer initializers (bug 115515).  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

constexpr int i = 5i; /* { dg-error "constexpr' integer initializer is not an integer constant expression" } */
