/* Test C2x attribute syntax: rejected in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

[[]]; /* { dg-error "attributes before C2X" } */

void f [[]] (void); /* { dg-error "attributes before C2X" } */
