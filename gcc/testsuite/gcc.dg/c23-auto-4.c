/* Test C23 auto.  -Wc11-c23-compat warning.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

auto x = 2; /* { dg-warning "ISO C does not support 'auto' type deduction before" } */
