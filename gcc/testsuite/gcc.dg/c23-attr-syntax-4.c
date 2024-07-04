/* Test C23 attribute syntax: diagnosed with -Wc11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

[[]]; /* { dg-warning "attributes before C23" } */

void f [[]] (void); /* { dg-warning "attributes before C23" } */
