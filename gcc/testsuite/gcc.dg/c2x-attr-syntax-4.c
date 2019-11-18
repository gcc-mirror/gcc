/* Test C2x attribute syntax: diagnosed with -Wc11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

[[]]; /* { dg-warning "attributes before C2X" } */

void f [[]] (void); /* { dg-warning "attributes before C2X" } */
