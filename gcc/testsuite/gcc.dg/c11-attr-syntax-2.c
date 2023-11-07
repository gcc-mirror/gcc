/* Test C23 attribute syntax: rejected in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

[[]]; /* { dg-warning "attributes before C23" } */

void f [[]] (void); /* { dg-warning "attributes before C23" } */
