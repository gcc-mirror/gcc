/* Test C2x auto.  -Wc11-c2x-compat warning.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

auto x = 2; /* { dg-warning "ISO C does not support 'auto' type deduction before" } */
