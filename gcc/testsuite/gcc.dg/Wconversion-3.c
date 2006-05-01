/* { dg-do compile } */
/* { dg-options "-O2 -Wconversion" } */

unsigned f(unsigned a) { return a + -1; }  /* { dg-warning "negative" } */

