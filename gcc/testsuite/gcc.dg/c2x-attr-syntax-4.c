/* Test C23 attribute syntax: diagnosed with -Wc11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

[[]]; /* { dg-warning "attributes before C23" } */

void f [[]] (void); /* { dg-warning "attributes before C23" } */
