/* { dg-do compile } */

void g() __attribute__((aligned(bar))); /* { dg-error "undeclared here|not declared" } */
